package litedram

import scala.collection.mutable
import scala.sys.process._
import java.nio.file.{Paths, Files, StandardCopyOption}
import java.util.UUID


import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.wishbone._

case class LiteDramConfig(
    fpgaSpeedGrade: Integer,
    module: DramModule,
    byteGroups: Integer,
    numRanks: Integer,
    phyType: PhyType.Value,
    inputClkFreq: Integer,
    sysClkFreq: Integer,
    ioDelayClkFreq: Integer,
    cmdLatency: Integer = 0,
    rttNom: Integer = 60,
    rttWr: Integer = 60,
    rOn: Integer = 34,
    cmdBufferDepth: Integer = 16
)

object PhyType extends Enumeration {
    val V7DdrPhy = Value("V7DDRPHY")
}

sealed trait DramModule {
    def memType: String
    def genPython: String
}

case class Ddr3Module (
    banks: Integer,
    rows: Integer,
    cols: Integer,
    tRefi: Double,
    tWtr: Double,
    tRrd: Double,
    tRp: Double,
    tRcd: Double,
    tWr: Double,
    tRfc: Double,
    tFaw: Double,
    tRas: Double
) extends DramModule  {
    val tCcd = 4
    val tZqcs = (64, 80)

    def memType: String = "DDR3"

    def genPython: String = {
        s"""class CustomModule(DDR3Module):
           |    nbanks = ${banks}
           |    nrows  = ${rows}
           |    ncols  = ${cols} 
           |    technology_timings = _TechnologyTimings(
           |        tREFI=${tRefi},
           |        tWTR=(4, ${tWtr}),
           |        tCCD=(${tCcd}, None),
           |        tRRD=(4, ${tRrd}),
           |        tZQCS=(${tZqcs._1}, ${tZqcs._2}))
           |    speedgrade_timings = {
           |        "default": _SpeedgradeTimings(
           |            tRP=${tRp},
           |            tRCD=${tRcd},
           |            tWR=${tWr},
           |            tRFC=(None, ${tRfc}),
           |            tFAW=(None, ${tFaw}),
           |            tRAS=${tRas})
           |    }""".stripMargin
    }
}

sealed trait UserPort {
    def genYaml: String
    def getIo: Bundle
    def renameIo(io: Bundle, prefix: String)
}

case class AxiPort(config: Axi4Config) extends UserPort {
    require(config.idWidth > 0, "AXI id width must be non-zero width")
    require(!config.useProt, "LiteDram does not support AXI prot bits")
    require(!config.useCache, "LiteDram does not support AXI cache bits")
    require(!config.useLock, "LiteDram does not support AXI lock bits")
    require(!config.useQos, "LiteDram does not support AXI qos bits")
    require(!config.useRegion, "LiteDram does not support AXI region bits")

    def genYaml: String = {
        s"""{
           |            "type": "axi",
           |            "id_width": ${config.idWidth},
           |            "data_width": ${config.dataWidth}
           |        },""".stripMargin
    }

    def getIo: Bundle = slave(Axi4(config))

    def renameIo(io: Bundle, prefix: String) {
        val axi = io.asInstanceOf[Axi4]
        axi.aw.flatten.foreach(signal => signal.setName(prefix + "aw" + signal.getPartialName()))
        axi.w.flatten.foreach(signal => signal.setName(prefix + "w" + signal.getPartialName()))
        axi.b.flatten.foreach(signal => signal.setName(prefix + "b" + signal.getPartialName()))
        axi.ar.flatten.foreach(signal => signal.setName(prefix + "ar" + signal.getPartialName()))
        axi.r.flatten.foreach(signal => signal.setName(prefix + "r" + signal.getPartialName()))
    }
}

case class WishbonePort(config: WishboneConfig) extends UserPort {
    require(!config.useSTALL, "LiteDram does not support Wishbone STALL")
    require(!config.useLOCK, "LiteDram does not support Wishbone LOCK")
    require(config.selWidth == config.dataWidth / 8,
        "LiteDram uses one SEL bit per byte")

    def genYaml: String = {
        s"""{
            |            "type": "wishbone",
            |            "data_width": ${config.dataWidth}
            |        },""".stripMargin
    }   

    def getIo: Bundle = slave(Wishbone(config))

    def renameIo(io: Bundle, prefix: String) {
        val wb = io.asInstanceOf[Wishbone]
        wb.CYC.setName(prefix + "cyc")
        wb.STB.setName(prefix + "stb")
        wb.ACK.setName(prefix + "ack")
        wb.WE.setName(prefix + "we")
        wb.ADR.setName(prefix + "adr")
        wb.SEL.setName(prefix + "sel")
        wb.DAT_MOSI.setName(prefix + "dat_w")
        wb.DAT_MISO.setName(prefix + "dat_r")
        if (config.useERR) {
            wb.ERR.setName(prefix + "err")
        }
    }
}

class GenerateLogger extends ProcessLogger {
    def out(s: => String) {
        SpinalInfo(s)
    }

    def err(s: => String) {
        SpinalInfo(s)
    }

    def buffer[T](f: => T): T = f
}

object LiteDram {
    var id = 0

    private def genId: Integer = {
        val temp = id
        id = id + 1
        temp
    }

    private def genScript(config: LiteDramConfig): String = {
        s"""import litedram.modules
           |from litedram.modules import (
           |   DDR3Module,
           |   _TechnologyTimings,
           |   _SpeedgradeTimings)
           |
           |${config.module.genPython}
           |
           |litedram.modules.CustomModule = CustomModule
           |import litedram.gen
           |litedram.gen.main()""".stripMargin
    }

    private def genYaml(config: LiteDramConfig, userPorts: Seq[UserPort]): String = {
        val userPortsYaml = userPorts.zipWithIndex.map((p) => 
            s"""        "${p._2}": ${p._1.genYaml}"""
        ).mkString("\n")
        s"""{
           |    # General
           |    "cpu": null, # no CPU
           |    "speedgrade": ${config.fpgaSpeedGrade}, # FPGA speedgrade
           |    "memtype": ${config.module.memType}, # DRAM type
           |
           |    # PHY
           |    "cmd_latency": ${config.cmdLatency}, # Additional cmd latency
           |    "sdram_module": "CustomModule", # Spinal generated module
           |    "sdram_module_nb": ${config.byteGroups}, # Number of byte groups
           |    "sdram_rank_nb": ${config.numRanks}, # Number of ranks
           |    "sdram_phy": "${config.phyType}", # Type of PHY
           |
           |    # Electrical
           |    "rtt_nom": "${config.rttNom}ohm", # Nominal termination
           |    "rtt_wr": "${config.rttWr}ohm", # Write termination
           |    "ron": "${config.rOn}ohm", # Output driver impedance
           |
           |    # Frequency
           |    "input_clk_freq": ${config.inputClkFreq}, # Input clock frequency
           |    "sys_clk_freq": ${config.sysClkFreq}, # System clock frequency (DDR_clk = 4 * sys_clk)
           |    "iodelay_clk_freq": ${config.ioDelayClkFreq}, # IODELAYs reference clock frequency
           |
           |    # Core
           |    "cmd_buffer_depth": ${config.cmdBufferDepth},
           |
           |    # User Ports
           |    "user_ports": {
           |${userPortsYaml}
           |    }
           |}""".stripMargin
    }

    private def generatedName(id: Integer): String = {
        s"LiteDramCore${id}"
    }

    private def generate(id: Integer, config: LiteDramConfig, userPorts: Seq[UserPort]) {
        val targetDir = Paths.get("target", "litedram", s"litedram${id}")
        SpinalInfo(s"Generating ${generatedName(id)} in directory: ${targetDir.toString}")
        targetDir.toFile().mkdirs()
        val scriptFile = targetDir.resolve("gen.py")
        Files.write(scriptFile, genScript(config).getBytes)
        val yamlFile = targetDir.resolve("config.yaml")
        Files.write(yamlFile, genYaml(config, userPorts).getBytes)
        val genCmd = Process(Seq(
            "python3",
            "gen.py",
            "--no-compile-gateware",
            "--no-compile-software",
            "--module-name",
            generatedName(id),
            "config.yaml"), targetDir.toFile)
        if (genCmd.!(new GenerateLogger) != 0) {
            SpinalError("LiteDram generation failed")
        }
    }

    private def copyGeneratedToTarget(id: Integer, targetDir: String) {
        Paths.get(targetDir).toFile().mkdirs()
        val outputPath = Paths.get("target", "litedram", s"litedram${id}",
            "build", "gateware", generatedSource(id))
        SpinalInfo(s"Copying ${outputPath.toString} to ${targetDir}")
        Files.copy(
            outputPath,
            Paths.get(targetDir, generatedSource(id)),
            StandardCopyOption.REPLACE_EXISTING)
    }

    private def generatedSource(id: Integer): String = {
        s"${generatedName(id)}.v"
    }
}

class LiteDram(config: LiteDramConfig, userPorts: Seq[UserPort]) extends BlackBox {
    require(!userPorts.isEmpty, "LiteDram must have at least one user port")

    val io = new Bundle {
        val clk = in Bool
        val rst = in Bool
        val userPort = userPorts.map(p => p.getIo)
    }

    noIoPrefix()
    private def renameIO = {
        io.userPort.zip(userPorts).zipWithIndex.map(pi =>
            pi._1._2.renameIo(pi._1._1, s"user_port_${pi._2}_"))
    }

    val id = LiteDram.genId
    setBlackBoxName(LiteDram.generatedName(id))
    addRTLPath(LiteDram.generatedSource(id))
    addPrePopTask(() => {
        LiteDram.generate(id, config, userPorts)
        LiteDram.copyGeneratedToTarget(id, globalData.phaseContext.config.targetDirectory)
        renameIO
    })
}

class TopLevel extends Component {
    val config = LiteDramConfig(
        fpgaSpeedGrade = -2,
        module = Ddr3Module(
            banks = 8,
            rows = 32768,
            cols = 1024,
            tRefi = 64e6/8192.0,
            tWtr = 7.5,
            tRrd = 7.5,
            tRp = 13.75,
            tRcd = 13.75,
            tWr = 15,
            tRfc = 260,
            tFaw = 40,
            tRas = 35),
        byteGroups = 8,
        numRanks = 2,
        phyType = PhyType.V7DdrPhy,
        inputClkFreq = 100000000,
        sysClkFreq = 100000000,
        ioDelayClkFreq = 200000000
    )
    val axiConfig = Axi4Config(addressWidth = 32, dataWidth = 32,
        idWidth = 1, useProt = false, useCache = false,
        useLock = false, useRegion = false, useQos = false)
    val wbConfig = WishboneConfig(addressWidth = 30, dataWidth = 32,
        selWidth = 4, useERR = true)

    val io = new Bundle {
        val clk = in Bool
        val rst = in Bool
        val axi = slave(Axi4(axiConfig))
        val wb = slave(Wishbone(wbConfig))
    }

    val dram = new LiteDram(config, Seq(AxiPort(axiConfig), WishbonePort(wbConfig)))
    dram.io.clk := io.clk
    dram.io.rst := io.rst
    io.axi >> dram.io.userPort(0).asInstanceOf[Axi4]
    io.wb >> dram.io.userPort(1).asInstanceOf[Wishbone]
}

object SpinalLiteDram {
    def main(args: Array[String]) {
        val spinalConfig = SpinalConfig(
            targetDirectory = "rtl"
        )
        spinalConfig.generateVerilog(new TopLevel)
    }
}
