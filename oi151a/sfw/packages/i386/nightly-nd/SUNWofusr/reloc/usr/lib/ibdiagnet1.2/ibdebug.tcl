##############################
### Initialize Databases
##############################
# InitializeIBDIAG                               
# InitializeINFO_LST
# InitializeOutputFile
# ParseOptionsList

##############################
### Initial and final actions
##############################
# InitializeIBIS
# SetPortNDevice
# SetTopologyNSysName
# DeleteOldFiles
# StartIBDIAG
# FinishIBDIAG

##############################
### MADs handling
##############################
# SmMadGetByDr
# SmMadGetByLid
# GetPmList

##############################
### Farbic Discovery
##############################
# DiscoverFabric
# DiscoverPath
# DiscoverHiddenFabric
# SetNeighbor
# Bool_DuplicateGuids

##############################
### Farbic Qualities Reports
##############################
# DumpBadLidsGuids
# DumpBadLinksLogic
# RereadLongPaths
# PMCounterQuery
# RunPkgProcs

##### UP TO HERE

##############################
### GENERAL PURPOSE PROCs
##############################
# BoolWordInList
# RemoveElementFromList
# GetWordAfterFlag
# Bar
# AddZeroes
# RemoveZeroes
# Hex2Bin
# GetLengthMaxWord
# AddSpaces
# ProcName
# groupNumRanges
# groupingEngine
# compressNames

##############################
### Handling Duplicated Guids
##############################
# AdvncedMaskGuid
# GetCurrentMaskGuid
# BoolIsMaked
# GetRealPort

##############################
### Handling bad links
##############################
# PathIsBad
# DetectBadLinks
# ComparePMCounters
# DumpBadLinks
# RemoveDirectPath

##############################
### SM handling
##############################
# CheckSM
# DumpSMReport

##############################
### handling topology file
##############################
# MatchTopology
# DumpTopologyMatching
# ArrangeDR
# DrPath2Name
# linkNamesGet
# GetArgvPortNames
# Name2Lid
# DumpFabQualities

##############################
### format fabric info
##############################
# GetDeviceFullType
# GetEntryPort
# GetParamValue
# FormatInfo

##############################
### ouput fabric info
##############################
# linkAtPathEnd
# lstInfo
# writeDBFile
# writeLstFile
# writeNeighborFile
# writeMasksFile
# writeSMFile
# writePMFile
# writeFdbsFile
# writeMcfdbsFile
# writeTopologyFileAndIBNLs

######################################################################
### Initialize Databases
######################################################################
#  NAME         InitializeIBDIAG
#  FUNCTION set the inital enviorment values
#  OUTPUT   NULL
proc InitializeIBDIAG {} {
    global G argv argv0 InfoArgv INFO_LST MASK

    ### InitializeIBDIAG - Set general vars
    set G(var:version.num)          1.3.0rc14
    set G(var:tool.name)            [file rootname [file tail $argv0]]
    set G(var:start.clock.seconds)  [clock seconds]
    set G(var:desc.local.dev)       "The Local Device"

    ### InitializeIBDIAG - Configuration of constants
    ## Configuration of constants - Step1.0: Config lists of vars
    set G(var:list.files.extention) "lst fdbs mcfdbs log neighbor masks sm pm mcgs pkey db"
    set G(var:list.pm.counter)      "symbol_error_counter link_error_recovery_counter\
      link_down_counter port_rcv_errors port_xmit_discard vl15_dropped\
      port_rcv_constraint_errors local_link_integrity_errors\
      port_xmit_constraint_errors excesive_buffer_errors port_xmit_data\
	  port_rcv_data port_xmit_pkts port_rcv_pkts all"
    set G(var:list.skip.steps) "dup_guids zero_guids pm logical_state load_ibdm ipoib part all"

    ## Configuration of constants - Step2.0: Config badpath vars
    set G(var:badpath.maxnErrors)    3
    set G(var:badpath.retriesStart)  100
    set G(var:badpath.retriesEnd)    10000
    set G(var:badpath.retriesGrowth) 10

    ## Configuration of constants - Step3.0: Set maximum warnings/error reports for
    # topology matching, before notifing the user that his cluster is messed up
    set G(var:warn.long.matching.results) 20

    ## Configuration of constants - Step4.0: Config argv/ParseArgv related vars
    # The max/min values for integer-valued parameters
    set G(var:maximal.integer) 1000000
    set G(var:minimal.integer) -1000000

    ### InitializeIBDIAG - Reset array entries
    ## Reset array entries - Step1.0: Reset "data" entries, entries which change during
    # data gathering
    set G(data:list.direct.path) { "" }
    set G(data:list.node.guids) [list ]
    set G(data:list.port.guids) [list ]
    set G(data:list.links.not.active.logical.state) ""
    set G(data:list.bad.paths) ""

    set G(data:counter.SW) 0
    set G(data:counter.CA) 0

    ## Reset array entries - Step2.0: Reseting boolean vars
    set G(bool:bad.links.detected) 0
    set G(bool:topology.matched) 0
    set G(bool:hidden.fabric.discovered) 0
    set G(bool:sys.name.guessed) 0


    ### InitializeIBDIAG - Other
    set MASK(CurrentMaskGuid) 1

    source [file join [file dirname [info script]] ibdebug_if.tcl]
    uplevel \#0 source [file join [file dirname [info script]] git_version.tcl]
    fconfigure stdout -buffering none
    SetInfoArgv
    UpdateInfoArgv_Win
    return 0
}
#################################

#################################
#  NAME         InitializeINFO_LST
#  FUNCTION Initialize the INFO_LST array, which defined the specific way
#               to read and interpreted the result from MADS
#  INPUTS   NULL
#  OUTPUT   NULL
#  RESULT   the array INFO_LST is defined.
proc InitializeINFO_LST {} {
    global INFO_LST
    array set INFO_LST {
	Type      { -source NodeInfo -flag node_type -width 8 -substitution "1=CA 2=SW 3=Rt" -string 1 }
	Ports     { -source NodeInfo -flag num_ports -width 8 }
	SystemGUID  { -source NodeInfo -flag sys_guid  -width 64 }
	NodeGUID    { -source NodeInfo -flag node_guid -width 64 }
	PortGUID    { -source NodeInfo -flag port_guid -width 64 }
	DevID     { -source NodeInfo -flag device_id -width 16 }
	Rev       { -source NodeInfo -flag revision  -width 32 }
	PN     { -width 8 }
	PortNum      { -source NodeInfo -flag port_num_vendor_id -width 8 -offset 0:32}
	VenID     { -source NodeInfo -flag port_num_vendor_id -width 24 -offset 8:32}
	NodeDesc    { -source NodeDesc -flag description -width words -string 1 }
	LID       { -source PortInfo -flag base_lid    -width 16 -fromport0 1 }
	PHY       { -source PortInfo -flag link_width_active -width 8 -substitution "1=1x 2=4x 4=8x 8=12x" -string 1 }
	LOG       { -source PortInfo -flag state_info1 -width 4 -offset 4:8 -substitution "1=DWN 2=INI 3=ARM 4=ACT" -string 1 }
	SPD         { -source PortInfo -flag link_speed  -width 4 -offset 0:8 -substitution "1=2.5 2=5 4=10" -string 1 }
	OpVL        { -source PortInfo -flag vl_enforce  -width 4 -offset 0:8 -substitution "1=0 2=1 3=3 4=7 5=14" -string 1 }
	PKey        { -source PortInfo -flag p_key_violations -width 16 -offset }
    }
    return 0
}
#################################

#################################
#  NAME         InitializeOutputFile
#  SYNOPSIS     InitializeOutputFile $_fileName
#  FUNCTION     open an output file for writing
#  INPUTS       file name
#  OUTPUT       file definition
proc InitializeOutputFile {_fileName} {
    global G

    ## Initialize file - Test1.0: Check if the extension is legit
    set ext [file extension $_fileName]
    if {![info exists G(outfiles,$ext)]} {
	inform "-E-outfile:not.valid" -file0 $outfile
    }

    ## Initialize file - Test2.0: Check if the file is writable
    set outfile $G(outfiles,[file extension $_fileName])
    if { [file exists $outfile] && ! [file writable $outfile] } {
	inform "-W-outfile:not.writable" -file0 $outfile -file1 $outfile.[pid]
	append G(outfiles,$ext) ".[pid]"
    }

    inform "-V-outfiles:$ext"
    return [open $G(outfiles,$ext) w]
}
#################################

#################################
#  NAME         ParseOptionsList
#  SYNOPSIS     ParseOptionsList list
#  FUNCTION     defines the database (in uplevel) bearing the values of the
#               options in a list
#  INPUTS       list of options (strings starting with "-") and their
#               values
#  OUTPUT       NULL
#  RESULT       the array $cfg() is defined in the level calling the procedure
#        $cfg(option) is the value of the option
## DZ: TODO review this procedure
proc ParseOptionsList { _options } {
    catch { uplevel unset cfg }
    set cfgArrayList ""
    while { [llength $_options] > 0 } {
	set flag  [lindex $_options 0]
	set value [list [lindex $_options 1]]
	set _options  [lreplace $_options 0 1]
	if {[regexp {^\-([^ ]+)$} $flag . flag ]} {
	    lappend cfgArrayList "$flag" "$value"
	} else {
	    return -code 1 -errorcode $flag
	}
    }
    uplevel array set cfg \"$cfgArrayList\"
    return 0
}
#################################

######################################################################
### Initial and final actions
######################################################################
#  NAME         InitializeIBIS
#  SYNOPSIS InitializeIBIS
#  FUNCTION Initialize ibis
#  INPUTS   NULL
#  OUTPUT   the result of the command "ibis_get_local_ports_info"
#  RESULT       ibis.log fn and path are defined, ibis transaction_timeout is defined
proc InitializeIBIS {} {
    global tcl_platform env G
    set outDir $G(argv:out.dir)
    set toolName $G(var:tool.name)

    ### InitializeIBIS - Handle ibis basic params
    ## Initialize ibis - Step1.0: set transaction_timeout
    catch { ibis_set_transaction_timeout 100 }

    ## Initialize ibis - Step1.1: set verbosity
    #ibis_set_verbosity 0xffff


    ### InitializeIBIS - Handle ibis log file
    ## Initialize ibis - Step2.0: Set ibis log directory
    set ibisOutDir $outDir

    ## Initialize ibis - Test1.0: Create the directory or check if its writable
    if {![file isdirectory $ibisOutDir]} {
	if {[catch {file mkdir $ibisOutDir} errMsg]} {
	    inform "-E-ibis:could.not.create.directory" -value $ibisOutDir -errMsg $errMsg
	}
    } elseif {![file writable $ibisOutDir]} {
	inform "-E-ibis:directory.not.writable" -value $ibisOutDir
    }

    ## Initialize ibis - Step2.1: Create ibis log file
    set ibisLogFile ${toolName}_ibis.log
    if {[file exists $ibisOutDir/$ibisLogFile] && (![file writable $ibisOutDir/$ibisLogFile])} {
	set ibisLogFile $ibisLogFile.[pid]
    }
    if {[file exists $ibisOutDir/$ibisLogFile]} {
	if {![file writable $ibisOutDir/$ibisLogFile]} {
	    if {![file writable $ibisOutDir/$ibisLogFile]} {
		catch {set ibisLogFd [open $ibisOutDir/$ibisLogFile w]} errMsg
		inform "-E-ibis:file.not.writable" -value $ibisOutDir/$ibisLogFile -errMsg $errMsg
	    }
	}
    }
    inform "-V-ibis.ibis.log.file" -value $ibisOutDir/$ibisLogFile

    ## Initialize ibis - Step2.2: Set ibis log file (created in Steps2.0-2.1)
    ibis_opts configure -log_file $ibisOutDir/$ibisLogFile


    ### InitializeIBIS - Handle simulator
    if {[info exists env(IBMGTSIM_DIR)]} {
	ibis_opts configure -log_file [file join $env(IBMGTSIM_DIR) ibis.log]
    }

    ### InitializeIBIS - Initialize ibis: ibis_init,ibis_get_local_ports_info
    ## Initialize ibis - Step 3.0: ibis_init
    if {[catch { ibis_init } ErrMsg]} {
	inform "-E-ibis:ibis_init.failed" -errMsg "$ErrMsg"
    }

    ## Initialize ibis - Step3.1: ibis_get_local_ports_info
    if {[catch { ibis_get_local_ports_info } ibisInfo ]} {
	if { $ibisInfo != "" } {
	    inform "-E-ibis:ibis_get_local_ports_info.failed" -errMsg "$ibisInfo"
	}
    } else {
	inform "-V-ibis:ibis_get_local_ports_info" -value "$ibisInfo"
    }

    ## Initialize ibis - Test2.0: In case no HCA present or the driver is not working properly
    if { $ibisInfo == "" } {
	inform "-E-ibis:no.hca"
    }
    return $ibisInfo
}
#################################

#################################
#  NAME         SetPortNDevice
#  SYNOPSIS SetPortNDevice $_ibisInfo
#  FUNCTION Sets the locat exit port and the local exit device
#               by parsing ibisInfo (the output of ibis_get_local_ports_info)
#  INPUTS       The result from : ibis_get_local_ports_info"
#  OUTPUT   NULL
#  RESULT       set G(argv:port.num), G(data:root.port.guid) and G(data:root.port.lid).
proc SetPortNDevice {_ibisInfo} {
    global G PORT_HCA

    ### SetPortNDevice - Pre Settings
    ## Pre Settings - Step1.0: Local vars
    set count_device 1
    set ibisInfo $_ibisInfo
    set toolName $G(var:tool.name)
    set bool_oldIBIS 0
    set bool_oldOSM 0

    ## Pre Settings - Step1.1: Set vars by G(argv:*)
    set bool_smp $G(argv:symmetric.multi.processing)
    set bool_portNumSet [info exists G(argv:port.num)]
    set bool_devNumSet  [info exists G(argv:dev.idx)]
    if {$bool_portNumSet} {
	set argv_portNum $G(argv:port.num)
    }
    if {$bool_devNumSet} {
	set argv_devIdx $G(argv:dev.idx)
    }


    ### SetPortNDevice - IBIS and OSM
    ## IBIS and OSM - Step1.0: Determine version of osm and ibis
    if {[llength [lindex $ibisInfo 0]] < 4} {
	set bool_oldIBIS 1
	inform "-W-loading:old.ibis.version"
    } else {
	foreach ibisEntry $ibisInfo {
	    scan $ibisEntry {%s %s %s %s} portGuid portLid portState portNum
	    if {($portNum != 1) && ($portNum != 2)} {
		set bool_oldOSM 1
		break;
	    }
	}
    }

    ## IBIS and OSM - Step1.1: Handle old osm version: ignore PN entries in ibisInfo
    if {$bool_oldOSM} {
	inform "-W-loading:old.osm.version"
	set tmp_ibisInfo ""
	foreach ibisEntry $ibisInfo {
	    lappend tmp_ibisInfo [lrange $ibisEntry 0 2]
	}
	set ibisInfo $tmp_ibisInfo
    }

    ## IBIS and OSM - Step1.2: Handle Gen2 (or higher): ignore the default port
    if {[llength $ibisInfo] > 1} {
	if {[lsearch -start 1 $ibisInfo [lindex $ibisInfo 0]]!= -1} {
	    set ibisInfo [lrange $ibisInfo 1 end]
	}
    }


    ### SetPortNDevice - Set PORT_HCA according to $ibisInfo
    ## Set PORT_HCA - Case1.0: Old ibis/osm => assume only one HCA present on the local host
    if {$bool_oldOSM || $bool_oldIBIS } {
	for {set portNumIndx 0} {$portNumIndx < [llength $ibisInfo]} {incr portNumIndx} {
	    set listEntry [lindex $ibisInfo $portNumIndx]
	    scan $listEntry {%s %s %s} portGuid portLid portState
	    set PORT_HCA($count_device.[expr 1 + $portNumIndx]:portGuid)  $portGuid
	    set PORT_HCA($count_device.[expr 1 + $portNumIndx]:portLid)   $portLid
	    set PORT_HCA($count_device.[expr 1 + $portNumIndx]:portState) $portState
	}
    } else {
	set prev_portNum 0
	for {set portNumIndx 0} {$portNumIndx < [llength $ibisInfo]} {incr portNumIndx} {
	    set listEntry [lindex $ibisInfo $portNumIndx]
	    scan $listEntry {%s %s %s %s} portGuid portLid portState portNum
	    if {$prev_portNum >= $portNum} {
		incr count_device
	    }
	    set prev_portNum $portNum
	    set PORT_HCA($count_device.$portNum:portGuid)  $portGuid
	    set PORT_HCA($count_device.$portNum:portLid)   $portLid
	    set PORT_HCA($count_device.$portNum:portState) $portState
	}
    }


    ### SetPortNDevice - Port and Dev set according to specified/unspecified device index and port number
    ## Port and Dev - Case1.0: port and device specified
    if {$bool_portNumSet && $bool_devNumSet} {
	## Port and Dev - Test1.0: Check if the device index exists
	if {$argv_devIdx > $count_device} {
	    inform "-E-localPort:dev.not.found" -value "$argv_devIdx" -maxDevices $count_device
	}

	## Port and Dev - Test1.1: Check if port number exists on the specified device
	if {![info exists PORT_HCA($argv_devIdx.$argv_portNum:portGuid)]} {
	    inform "-E-localPort:port.not.found.in.device" -flag "-p" -port $argv_portNum -device $argv_devIdx
	}

	## Port and Dev - Test1.2: Check the port state
	set portState $PORT_HCA($argv_devIdx.$argv_portNum:portState)
	if { $portState == "DOWN" && ( $toolName != "ibcfg" )} {
	    inform "-E-localPort:local.port.of.device.down" -port $argv_portNum -device $argv_devIdx
	}

	## Port and Dev - Test1.3: Special case for ibdiagpath, ignore
	if { ( $portState != "ACTIVE" ) && ( $toolName == "ibdiagpath" ) } {
	    # -smp flag allow ibdiagpath to work with INIT state
	    if {!($bool_smp || ($portState == "ACTIVE")) } {
		inform "-E-localPort:local.port.of.device.not.active" \
		    -port $argv_portNum -state $portState -device $argv_devIdx
	    }
	}
    }

    ## Port and Dev - Case2.0: Only device index was specified
    if {!($bool_portNumSet) && $bool_devNumSet} {
	## Port and Dev - Test2.0: Check if the device index exists
	if {$argv_devIdx > $count_device} {
	    inform "-E-localPort:dev.not.found" -value "$argv_devIdx" -maxDevices $count_device
	}
	set bool_allPortsDown 1
	set count_upPorts 0
	foreach arrayEntry [lsort [array names PORT_HCA $argv_devIdx.*:portState]] {
	    set portState $PORT_HCA($arrayEntry)
	    if { $portState == "DOWN" } {continue;}
	    if { ( $portState != "ACTIVE" ) && ( $toolName == "ibdiagpath" ) } {
		if {!($bool_smp || ($portState == "ACTIVE")) } {
		    continue;
		}
	    }
	    incr count_upPorts
	    if {$bool_allPortsDown} {
		set saveEntry $arrayEntry
		set bool_allPortsDown 0
	    }
	}

	## Port and Dev - Test2.1: Check the ports state on the specified device
	if {$bool_allPortsDown} {
	    switch $toolName {
		"ibcfg" {
		    set argv_portNum 1
		}
		default {
		    inform "-E-localPort:all.ports.of.device.down" -device $argv_devIdx
		}
	    }
	} else {
	    set argv_portNum [lindex [split $saveEntry ". :"] 1]
	}

	## Port and Dev - Test2.2: Inform the total of available ports
	if {$count_upPorts > 1} {
	    inform "-W-localPort:few.ports.up" -flag "-p" -port $argv_portNum -device $argv_devIdx
	} else {
	    inform "-I-localPort:one.port.up" -port $argv_portNum
	}
    }

    ## Port and Dev - Case3.0: Only port number was specified
    if {$bool_portNumSet && !($bool_devNumSet)} {
	## Port and Dev - Test3.1: Check if the port index exists (on any of the host HCAs)
	if {[llength [array names PORT_HCA *.$argv_portNum:portState]] == 0} {
	    inform "-E-localPort:port.not.found" -value $argv_portNum
	}
	set bool_allPortsDown 1
	set saveState "DOWN"
	set count_UpDevices 0
	foreach arrayEntry [lsort [array names PORT_HCA *.$argv_portNum:portState]] {
	    set portState $PORT_HCA($arrayEntry)
	    if { $portState == "DOWN" } {continue;}
	    set saveState $portState
	    if { ( $portState != "ACTIVE" ) && ( $toolName == "ibdiagpath" ) } {
		if {!$bool_smp || ($portState != "INIT") } {
		    continue;
		}
	    }

	    if {$bool_allPortsDown} {
		set saveState $portState
		set argv_devIdx [lindex [split $arrayEntry ". :"] 0]
	    }
	    incr count_UpDevices
	    set bool_allPortsDown 0
	}

	## Port and Dev - Test3.2: Check the ports state on the specified device
	if {$bool_allPortsDown} {
	    switch $toolName {
		"ibdiagpath" {
		    inform "-E-localPort:local.port.not.active" \
			-port $argv_portNum -state $saveState
		}
		"ibcfg" {
		    set argv_devIdx 1
		}
		default {
		    inform "-E-localPort:local.port.down" -port $argv_portNum
		}
	    }
	}

	## Port and Dev - Test3.3: Inform the total of available devices
	if {$count_UpDevices > 1} {
	    inform "-W-localPort:few.devices.up" -flag "-p" -port $argv_portNum -device $argv_devIdx
	} elseif {$count_device > 1} {
	    inform "-I-localPort:using.dev.index" -device $argv_devIdx
	}
    }

    ## Port and Dev - Case4.0: Neither port num or device index were requested
    if {!($bool_portNumSet) && !($bool_devNumSet)} {
	set bool_allPortsDown 1
	set saveState "DOWN"
	set count_upPorts 0
	foreach arrayEntry [lsort [array names PORT_HCA *.*:portState]] {
	    set portState $PORT_HCA($arrayEntry)
	    if { $portState == "DOWN" } {continue;}
	    set saveState $portState
	    if { ( $portState != "ACTIVE" ) && ( $toolName == "ibdiagpath" ) } {
		if {!$bool_smp || ($portState != "INIT") } {
		    continue;
		}
	    }
	    if {$bool_allPortsDown} {
		set argv_devIdx  [lindex [split $arrayEntry ". :"] 0]
		set argv_portNum [lindex [split $arrayEntry ". :"] 1]
	    }
	    incr count_upPorts
	    set bool_allPortsDown 0
	}
	if {$bool_allPortsDown} {
	    if {$count_device > 1} {
		set informMsg "-E-localPort:all.ports.down.mulitple.devices"
	    } else {
		set informMsg "-E-localPort:all.ports.down"
	    }
	    switch $toolName {
		"ibdiagpath" {
		    inform $informMsg
		}
		"ibcfg" {
		    set argv_devIdx 1
		    set argv_portNum 1
		}
		default {
		    inform $informMsg
		}
	    }
	}

	if {$count_upPorts > 1} {
	    inform "-W-localPort:few.ports.up" -flag "-p" -port $argv_portNum -device $argv_devIdx
	} else {
	    inform "-I-localPort:one.port.up" -port $argv_portNum
	}
    }

    ### SetPortNDevice - Setting G with port/dev info
    ## Setting G with port/dev info - Step1.0: Set port/dev index
    set G(argv:port.num) $argv_portNum
    set G(argv:dev.idx) $argv_devIdx

    ## Setting G with port/dev info - Step1.1: Set port GUID & LID
    set G(data:root.port.guid) $PORT_HCA($argv_devIdx.$argv_portNum:portGuid)
    set G(data:root.port.lid)  $PORT_HCA($argv_devIdx.$argv_portNum:portLid)

    ## Setting G with port/dev info - Test1.0: Zero guid is disallowed
    if {$G(data:root.port.guid) == "0x0000000000000000"} {
	inform "-E-localPort:port.guid.zero"
    }
    ## Setting G with port/dev info - Test2.0: try to run 'ibis_set_port'
    if {[catch {ibis_set_port $G(data:root.port.guid)} e]} {
	inform "-E-localPort:enable.ibis.set.port"
    }
    ## Setting G with port/dev info - Step2.0: Inform for -dr port derived settings
    if {[info exists G(-p.set.by.-d)]} {
	inform "-I-localPort:is.dr.path.out.port" -port $argv_portNum
    }
    return 0
}

##############################

##############################
#  NAME         SetTopologyNSysName
#  SYNOPSIS SetTopologyNSysName
#  FUNCTION Sets and checks the topology file and local system name
#  INPUTS       NULL
#  OUTPUT   NULL
#  RESULT       set G(argv:sys.name)
proc SetTopologyNSysName {} {
    global G

    ### SetTopologyNSysName - Pre Settings
    ## Pre Settings - Step1.0: Local vars
    set HCAnames ""

    ## Pre Settings - Step1.0: Set vars by G(argv:*)
    set bool_topoFileSet [info exists G(argv:topo.file)]
    set bool_sysNameSet [info exists G(argv:sys.name)]


    ### SetTopologyNSysName - Pre Testing
    ## Pre Testing - Test1.0: Run this procedure only if a topology is specified
    if {!$bool_topoFileSet} {
	return 1
    }

    ## Pre Testing - Test2.0: Run this procedure only if no skip ibdm request made
    if {[CheckSkipStatus load_ibdm]} {
	return 1
    }


    ### SetTopologyNSysName - Sys name retrive
    ## Sys name - Step1.0: Set list_names
    if {!$bool_sysNameSet} {
	## Sys name - Case1.0: Set list_names by 'hostname' and NodeDescMad
	set list_names [lindex [split [info hostname] .] 0]
	catch { append list_names " " [SmMadGetByDr NodeDesc -description {}] }
    } else {
	## Sys name - Case2.0: Set list_names by provided info (-s)
	set list_names $G(argv:sys.name)
    }

    ## Sys name - Step2.0: Retrive Nodes and System names from the provided topology
    array set TOPO_NODES [join [IBFabric_NodeByName_get $G(IBfabric:.topo)]]
    array set TOPO_SYS   [join [IBFabric_SystemByName_get $G(IBfabric:.topo)]]

    ## Sys name - Step3.0: Mark the fact that the sys name is guessed
    if {!$bool_sysNameSet} {
	set G(bool:sys.name.guessed) 1
    }

    ## Sys name - Step4.0: Find a name from list_names in the provided topology
    foreach name $list_names {
	if {[info exists TOPO_NODES($name)]} {
	    set G(argv:sys.name) $name
	    if { ! $bool_sysNameSet } {
		inform "-W-localPort:node.intelligently.guessed"
	    }
	    return 0
	} elseif {[info exists TOPO_SYS($name)]} {
	    set nodesNames [lsort -dictionary [IBSystem_NodeByName_get $TOPO_SYS($name)]]
	    set G(argv:sys.name) [lindex [lindex $nodesNames [expr $G(argv:dev.idx) -1]] 0]
	    if { !$bool_sysNameSet } {
		inform "-W-localPort:node.intelligently.guessed"
	    }
	    return 0
	}
    }

    ## Sys name - Step5.0: Handle the case that the local system name could not be
    # identified and advertise, only, the HCA-Sys names
    set list_sysNames [array names TOPO_SYS]
    foreach sysName $list_sysNames {
	set sysPointer $TOPO_SYS($sysName)
	foreach nodeName [IBSystem_NodeByName_get $sysPointer] {
	    if { [IBNode_type_get [lindex $nodeName 1]] != 1 } {
		lappend HCAnames $sysName
		break;
	    }
	}
    }
    ## Sys name - Step5.1: Inform if the provided sysName was illegal
    # or if none could be guessed
    if {$bool_sysNameSet} {
	inform "-E-argv:bad.sys.name" -flag "-s" -value $G(argv:sys.name) -names [lsort $HCAnames]
    } else {
	inform "-E-argv:unknown.sys.name" -names [lsort $HCAnames]
    }
    return 0
}
##############################

##############################
#  NAME         DeleteOldFiles   
#  FUNCTION Delete the old ibdiag files
#  INPUTS       NULL
#  OUTPUT   NULL
#  RESULT       ammm... the old ibdiag files are deleted
proc DeleteOldFiles {} {
    global G
    set list_filesNames [array names G "outfiles,*"]
    foreach fileName $list_filesNames  {
        scan [split $fileName .] {%s %s} . ext
        if {($ext == "log") || ($ext == "db")} {
	    continue
	}
	set tmp_fn $G($fileName)
	file delete -force $tmp_fn
    }
    return 0
}

##############################
#  SYNOPSIS StartIBDIAG
#  FUNCTION
#  executes the following initial actions when starting to run any tool:
#  - parsing the command line (running "ParseArgv")
#  - initianlize ibis:
#      ibis_opts configure -log_file (if necessary)
#      ibis_init,
#      ibis_get_local_ports_info
#  - parsing the result of ibis_get_local_ports_info:
#     - If local hca-index was specified, check that such device exists
#     - If local port-num was specified, check that this port is not DOWN
#       (ACTIVE, in case of ibdiagpath)
#     - If local port-num was not specified, set it to be the first not
#       DOWN (ACTIVE) port of the local device.
#  - if the above is OK, run ibis_set_port
#  - if a topology file is specified, check that the local system name is a
#     valid system name, or - if the latter was not specified - try to
#     guess it (if the host name or a word in the local node description
#     are valid system names).
#  INPUTS   NULL
#  OUTPUT   NULL
#  DATAMODEL
#  the procedure uses $env(IBMGTSIM_DIR) - if it exists, we are in simulation mode
#  the procedure uses the following global variables:
#     $G(argv:dev.idx) - the local-device-index
#     $G(argv:port.num) - the local-port-num (this var may also be set here)
#     $G(IBfabric:.topo) - the ibdm pointer to the fabric described in the topology file
#     $G(-p.set.by.-d) - if set, then the port-num was not explicitly
#       specified and it was set to be the output port of the direct route
#  the procedure also sets the global vars G(data:root.port.guid) and G(data:root.port.lid)
#  - the node-guid and LID of the local port.
proc StartIBDIAG {} {
    global G env tcl_patchLevel

    ### StartIBDIAG - Set the Tools Flags Array
    SetToolsFlags

    ### StartIBDIAG - Require the available packages
    catch {RequirePackage}

    ### StartIBDIAG - parsing command line arguments
    ParseArgv

    ### StartIBDIAG - Try to require IBDM
    RequireIBDM

    ### StartIBDIAG - Delete previous files
    DeleteOldFiles

    ### StartIBDIAG - Initialize ibis
    set ibisInfo [InitializeIBIS]

    ### StartIBDIAG - Setting the local port and device index
    SetPortNDevice $ibisInfo

    ### StartIBDIAG - Setting the local system name
    SetTopologyNSysName

    return 0
}

##############################
#  SYNOPSIS FinishIBDIAG
#  FUNCTION executes final actions for a tool:
#     - displays the "-I-done" info ("Done" + run time)
#     - exits the program
#  INPUTS   NULL
#  OUTPUT   NULL
#  DATAMODEL   I use $G(var:start.clock.seconds) to tell the total run time
proc FinishIBDIAG {} {
    global G

    ### FinishIBDIAG - Inform Fatel Error
    if { [info exists G(Fatal.err.found)] } {
	inform "-F-Fatal.header"
    }

    ### FinishIBDIAG - Inform running time
    inform "-I-done" $G(var:start.clock.seconds)

    ### FinishIBDIAG - Close ibdiag log file
    catch { close $G(logFileID) }
    exit 0
}

######################################################################
### Sending queries (MADs and pmGetPortCounters) over the fabric
######################################################################

##############################
#  SYNOPSIS     SmMadGetByDr mad cget args
#  FUNCTION
#  returns the info of the Direct Route Mad: sm${cmd}Mad getByDr $args.
#       It's recommanded to use this method to get MAS info- since it MAD
#       sending handles failures
#  INPUTS
#  $mad - the type of MAD to be sent - e.g., NodeInfo, PortInfo, etc.
#  $cget - the requested field of the mad ("dump" returns the all mad info)
#  $args - the direct route (and, optionally, the port) for sending the MAD
#  OUTPUT
#  the relevant field (or - all fields) of the MAD info
#  DATAMODEL
#  the procedure uses $G(argv:failed.retry) - for stopping failed retries
#  and $G(bool:bad.links.detected) to decide whether to run DetectBadLinks
proc SmMadGetByDr { mad cget args } {
    global G errorInfo

    ### SmMadGetByDr - Set the send and cget commands
    set getCmd [concat "sm${mad}Mad getByDr $args"]
    if {[regexp {^-} $cget]} {
	set cgetCmd "sm${mad}Mad cget $cget"
    } else {
	set cgetCmd "sm${mad}Mad $cget"
    }

    ### SmMadGetByDr - Send the mads (with up to $G(argv:failed.retry) retries)
    inform "-V-mad:sent" -command "$getCmd"
    set status -1
    for { set retry 0 } { $retry < $G(argv:failed.retry) } { incr retry } {
	if { [set status [eval $getCmd]] == 0 } {
	    incr retry
	    break;
	}
    }
    inform "-V-mad:received" -status $status -attempts $retry
    ### SmMadGetByDr - Handle the results
    if { $G(bool:bad.links.detected) && ( $status != 0 ) } {
	set res [DetectBadLinks $status "$cgetCmd" $mad $args]
	if {$res == -1} {
	    return -code 1 -errorcode $status
	} else {
	    return $res
	}
    } elseif { $status != 0 } {
	return -code 1 -errorcode $status
    } else {
	return [eval $cgetCmd]
    }
}

##############################

##############################
#  SYNOPSIS     SmMadGetByLid mad cget args
#  FUNCTION
#  returns the info of the lid based Mad: sm${cmd}Mad getByLid $args.
#  INPUTS
#  $mad - the type of MAD to be sent - e.g., NodeInfo, PortInfo, etc.
#  $cget - the requested field of the mad ("dump" returns the all mad info)
#  $args - the lid (and, optionally, the port) for sending the MAD
#  OUTPUT
#  the relevant field (or - all fields) of the MAD info
#  DATAMODEL
#  the procedure uses $G(argv:failed.retry) - for stopping failed retries
#  and $G(bool:bad.links.detected) to decide whether to run DetectBadLinks
proc SmMadGetByLid { mad cget args } {
    global G errorInfo

    ### SmMadGetByLid - Set the send and cget commands
    set getCmd [concat "sm${mad}Mad getByLid $args"]
    if {[regexp {^-} $cget]} {
	set cgetCmd "sm${mad}Mad cget $cget"
    } else {
	set cgetCmd "sm${mad}Mad $cget"
    }

    ### Send the mads (with up to $G(argv:failed.retry) retries)
    inform "-V-mad:sent" -command "$getCmd"
    set status -1
    for { set retry 0 } { $retry < $G(argv:failed.retry) } { incr retry } {
	if { [set status [eval $getCmd]] == 0 } { incr retry ; break; }
    }
    inform "-V-mad:received" -status $status -attempts $retry
    ### Handle the results
    if { $status != 0 } {
	return -code 1 -errorcode $status
    } else {
	return [eval $cgetCmd]
    }
}
##############################

##############################
#  SYNOPSIS     GetPmList Lid:Port
#  FUNCTION
#  returns the info of PM info request : pmGetPortCounters $Lid $Port
#  INPUTS
#  $LidPort - the lid and port number for the pm info request
#     format: lid:port (the semicolon - historic)
#  OUTPUT
#  the relevant PM (Performance Monitors) info for the $port at $lid
#  DATAMODEL
#  the procedure uses $G(argv:failed.retry) - for stopping failed retries
proc GetPmList { _lidPort } {
    global G
    set list_pm -1

    ## PM list get - Step1.0: Set lid, port and the pm command
    regexp {^(.*):(.*)$} $_lidPort . lid port
    if { $lid == 0 } {
	return
    }

    ## PM list get - Step1.1: Set pm command
    set cmd [concat "pmGetPortCounters $lid $port"]

    ## PM list get - Step2.0: Send the pm info request
    inform "-V-mad:sent" -command $cmd
    for { set retry 0 } { $retry < $G(argv:failed.retry) } { incr retry } {
	if { [regexp "ERROR" [set list_pm [join [eval $cmd]]]]==0 } {
	    break;
	}
    }
    inform "-V-mad:received" -attempts $retry

    ## PM list get - Step3.0: Handling the results
    if {[regexp "ERROR" $list_pm]} {
	return -code 1 -errorcode 1 -errorinfo "$list_pm"
    } else {
	return $list_pm
    }
}

######################################################################
### Farbic Discovery
######################################################################
#  SYNOPSIS    DiscoverFabric
#  FUNCTION & DATAMODEL
#  Using a BFS algorithm (staring at the local node), discovers the entire
#  fabric and sets up a few databases:
#       G(data:list.direct.path):
#       G(data:list.node.guids):
#       G(data:list.port.guids):
#       G(data:guid.by.dr.path.<DirectPath>)    : <PortGuid>
#       G(data:dr.path.to.guid.<PortGuid>)      : <DirectPath>
#       G(data:dr.path.to.node.<NodeGuid>)      : <DirectPath>
#       G(data:PortInfo.<NodeGuid>:<PN>)        : <SmPortInfoMad>
#       G(data:NodeGuid.<PortGuid>)          : <NodeGuid>
#       G(data:NodeInfo.<NodeGuid>):         : <smNodeInfoMad>
#       G(data:NodeDesc.<NodeGuid>)          :
#       G(data:PortGuid.<NodeGuid>:<PN>)     : <PortGuid>
#
#       Neighbor(<NodeGuid>:<PN>)       : <NodeGuid>:<PN>
#  
#       MASK(CurrentMaskGuid)           : <MaskGuid>
#       MASK(PortMask,<PortGuid>)       : <PortMask>
#       MASK(NodeMask,<NodeGuid>)       : <NodeMask>
#       MASK(PortGuid,<PortMask>)       : <PortGuid>
#       MASK(NodeGuid,<NodeMask>)       : <NodeGuid>
#
#       DUPandZERO(<PortGuid>,PortGUID) : <DirectPath>
#       DUPandZERO(<NodeGuid>,NodeGUID) : <DirectPath>
#       DUPandZERO(<value>,<ID>)        : <DirectPath>
#
#       SM(<SMstate>                    : <DirectPath>,SMpriority
#  
#       G(data:list.bad.paths) - list of second paths
#
#  INPUTS
#       PathLimit  - defined in which bad paths type the discovery should
#                   take place
#       startIndex - defined from which entry in G(data:list.direct.path) the
#                   discovery should take place
#  OUTPUT NULL
proc DiscoverFabric { _pathLimit {startIndex 0}} {
    global G DUPandZERO MASK Neighbor SM
    if {[info exists G(argv:ibdiag.db)]} {
        inform "-W-loading:external.ibdiag.db" -fn $G(argv:ibdiag.db)
        if {[catch {source $G(argv:ibdiag.db)} e]} {
            inform "-E-loading:old.ibdiag.db" -fn $G(argv:ibdiag.db) -errMsg $e
        }
        inform "-I-discover:discovery.status" -log
        return 0
    }
    
    inform "-V-discover:start.discovery.header"

    ### DiscoverFabric - Pre Settings
    ## Pre Settings - Step1.0: Local vars
    set index_dr $startIndex
    set bool_badPathFound 0
    while { $index_dr < [llength $G(data:list.direct.path)] } {
	### DiscoverFabric - Bad path
        if {$bool_badPathFound} {
	    ## Bad path - Step1.0: Add the path to data list
	    lappend G(data:list.bad.paths) $DirectPath
	    ## Bad path - Step1.1: Remove the dr path from all known DB
	    RemoveDirectPath $DirectPath
	    ## Bad path - Step1.2: Go back a step
	    incr index_dr -1
	    set bool_badPathFound 0
	    continue;
	}

	set DirectPath [lindex $G(data:list.direct.path) $index_dr]
	incr index_dr

	inform "-V-discover:discovery.status" -index $index_dr -path "$DirectPath"
	inform "-I-discover:discovery.status"

	### DiscoverFabric - Pre Query
	## Pre Query - Step1.0: If the path is worst then allowed, continue to next dr!
	if {[PathIsBad $DirectPath] > $_pathLimit} {
	    set bool_badPathFound 1
	    continue;
	}
	## Pre Query - Step2.0: If unable to get NodeInfo across $DirectPath, continue to next dr!
	if {[catch {set nodeInfo [SmMadGetByDr NodeInfo dump "$DirectPath"]}]} {
	    set bool_badPathFound 1
	    continue;
	}

	# Set nodeGuid,portGuid,EntryPort
	set nodeGuid [GetWordAfterFlag $nodeInfo "-node_guid"]
	set portGuid [GetWordAfterFlag $nodeInfo "-port_guid"]
	set entryPort [GetEntryPort $DirectPath -byNodeInfo $nodeInfo]

	### DiscoverFabric - Known Guids
	# Note: if changing a guid during discovery, could cause unreliable results
	# DZ TODO: set aside all the maybe HCA with 2 ports (Case2.0), and deal with them
	# at the end

	## Known Guids pre Query - Step1.0: Determine if the GUIDs are allready exists in DB
	set bool_nodeGuidknown [expr ([lsearch $G(data:list.node.guids) $nodeGuid]!= -1)]
	set bool_portGuidknown [expr ([lsearch $G(data:list.port.guids) $portGuid]!= -1)]
	set bool_skipGuids [CheckSkipStatus dup_guids]

	if {!$bool_skipGuids} {
	    set bool_duplicatePortGuid 0
	    set bool_duplicateNodeGuid 0

	    ## Known Guids - Case1.0: Known port GUID && Unknown node GUID
	    # No legal cases exists
	    if {$bool_portGuidknown && !$bool_nodeGuidknown} {
		set prev_drPath $G(data:dr.path.to.guid.$portGuid)
		## Known Guids - Case1.1: Duplicate port GUID Found!
		# No way to get the same port GUID from two diffrent nodes
		set bool_duplicatePortGuid 1
	    }
	    ## Known Guids - Case2.0: Unknown port GUID && Known node GUID
	    # Only one legal case: an HCA is connected with two ports
	    if {!$bool_portGuidknown && $bool_nodeGuidknown} {
		# Get the old port GUID, in order to get the old drPath
		set tmp_portGuid [lindex [array get G data:PortGuid.$nodeGuid:*] 1]
		set prev_drPath $G(data:dr.path.to.node.$nodeGuid)
		if {[catch {set tmp_type_1 [GetParamValue Type $prev_drPath]}]} {
		    set bool_badPathFound 1
		    continue;
		}
		if {[catch {set tmp_type_2 [GetParamValue Type $DirectPath]}]} {
		    set bool_badPathFound 1
		    continue;
		}                                                              
		## Known Guids - Case2.1: Duplicate node GUID Found!
		# previous and current port has diffrent types
		if {$tmp_type_1 != $tmp_type_2} {
		    set bool_duplicateNodeGuid 1
		}

		if {$tmp_type_2 == "CA"} {
		    if {[info exists Neighbor($nodeGuid:$entryPort)]} {
			## Known Guids - Case2.2: Duplicate node GUID Found!
			# There is allreay an HCA carring this node GUID and has this
			# entry port with a diffrent port GUID
			set bool_duplicateNodeGuid 1
		    }
		} elseif {[Bool_DuplicateGuids $nodeGuid $DirectPath $prev_drPath 1]} {
		    ## Known Guids - Case2.3: Duplicate node GUID Found!
		    # DZ: Q:I'm not sure way the above elseif is required, it seems
		    # that I immediately should set: "bool_duplicateNodeGuid <= 1"
		    # DZ: A: To support Sw that has more then one PG
		    set bool_duplicateNodeGuid 1
		}
	    }
	    ## Known Guids - Case3.0: Known port GUID && Known node GUID
	    if {$bool_portGuidknown && $bool_nodeGuidknown } {
		# HCA - Only "legit case" return to an HCA, through it's second
		# port, which is duplicated. Since we can't detemine if we visited
		# in an HCA before then PG and NG are dup
		# SW - Only if this is a SW which we visited before

		# Dr for the first encounter with the current PG
		set prev_drPath $G(data:dr.path.to.guid.$portGuid)
		# NG of current PG
		set prev_nodeGuid $G(data:NodeGuid.$portGuid)
		# PG of current NG (use only one because HCa has max of 2 ports)
		# and for switch its the same
		set tmp_portGuid [lindex [array get G data:PortGuid.$nodeGuid:*] 1]
		# Dr for the first encounter with the NG of the current PG
		set prev_drPath2 $G(data:dr.path.to.node.$nodeGuid)
		#set prev_drPath2 $G(data:dr.path.to.guid.$tmp_portGuid)
		if {[catch {set tmp_type_1 [GetParamValue Type $prev_drPath]}]} {
		    set bool_badPathFound 1
		    continue;
		}
		if {[catch {set tmp_type_2 [GetParamValue Type $DirectPath]}]} {
		    set bool_badPathFound 1
		    continue;
		}
		if {[catch {set tmp_type_3 [GetParamValue Type $prev_drPath2]}]} {
		    set bool_badPathFound 1
		    continue;
		}
		if {$tmp_type_2 == "CA"} {
		    if {$nodeGuid == $prev_nodeGuid } {
			## Known Guids - Case3.1: The current node GUID and previous node GUID
			# of the current port GUID are equal == The current PG and NG were once
			# belonged to the same Node
			if {$tmp_type_1 == "CA"} {
			    if {[info exists Neighbor($nodeGuid:$entryPort)]} {
				## Known Guids - Case3.1.1: Duplicate node and port Found!
				# Now in HCA previously in HCA which has this entry registered
				set bool_duplicatePortGuid 1
				set bool_duplicateNodeGuid 1
			    } else {
				## Known Guids - Case3.1.2: Duplicate node and port Found!
				# Possibly we reached an allready visited HCA with its PG duplicated
				# Or It's the same HCA but both it's port are with the same PG
				# the only way to be sure is to cahnge something in the current
				# HCA and check if the second one changed also. Since that is not
				# allowed Case3.1.2.1 will be the same as Case Case3.1.2.2
				# Case3.1.2.1
				set bool_duplicatePortGuid 1
				set bool_duplicateNodeGuid 1
				# Case3.1.2.2
				# set bool_duplicatePortGuid 1
				# set bool_duplicateNodeGuid 0

			    }
			} else {
			    ## Known Guids - Case3.1.3: Duplicate node and port Found!
			    #Case 1.2 - Now in HCA previously in SWITCH
			    set bool_duplicatePortGuid 1
			    set bool_duplicateNodeGuid 1
			}
		    } else {
			## Known Guids - Case3.2: MAYBE The current PG and NG were not belonged
			# to the same Node
			if {$tmp_type_3 == "CA"} {
			    # Case3.2.1 The node GUID was once belonging to an HCA
			    # ? Is it the same HCA ?
			    # We can only assume that it's the same HCA there for we treat it like
			    # it's not (DZ logic)
			}
			## Known Guids - Case3.2.1: Duplicate node and port Found!
			# Explained above
			set bool_duplicatePortGuid 1
			set bool_duplicateNodeGuid 1
		    }
		} else {
		    ## Known Guids - Case3.3: The current device is a switch
		    # if it's not the same node as before assume both are duplicate
		    # possible that only one is dup (ask DZ)
		    if {[Bool_DuplicateGuids $nodeGuid $DirectPath $prev_drPath2]} {
			set bool_duplicatePortGuid 1
			set bool_duplicateNodeGuid 1
		    }
		}
	    }
	    ### DiscoverFabric - Handle duplicate GUIDs
	    if {$bool_duplicatePortGuid || $bool_duplicateNodeGuid} {
		set nodeAllreadyMasked 0
		set portAllreadyMasked 0

		## Handle duplicate GUIDs - Case1.0: Duplicate port GUID which was masked before
		if {$bool_duplicatePortGuid} {
		    if {[info exists MASK(PortMask,$portGuid)]} {
			foreach portMask $MASK(PortMask,$portGuid) {
			    set prev_drPath $G(data:dr.path.to.guid.$portMask)
			    if {[Bool_SameDevice $DirectPath $prev_drPath] == 1} {
				set portAllreadyMasked 1
				set portGuid $portMask
				set bool_duplicatePortGuid 0
				break
			    }
			}
		    }
		}
		## Handle duplicate GUIDs - Case2.0: Duplicate node GUID which was masked before
		if {$bool_duplicateNodeGuid} {
		    if {[info exists MASK(NodeMask,$nodeGuid)]} {
			foreach nodeMask $MASK(NodeMask,$nodeGuid) {
			    set prev_drPath $G(data:dr.path.to.node.$nodeMask)
			    if {[Bool_SameDevice $DirectPath $prev_drPath] == 1} {
				set nodeAllreadyMasked 1
				set nodeGuid $nodeMask
				set bool_duplicateNodeGuid 0
				break
			    }
			}
		    }
		}

		## Handle duplicate GUIDs - Case3.0: Get node GUID from a masked port GUID
		if {$portAllreadyMasked} {
		    set nodeGuid $G(data:NodeGuid.$portGuid)
		    set nodeAllreadyMasked 1
		}

		## Handle duplicate GUIDs - Case4.0: Get port GUID from a masked node GUID
		if {$nodeAllreadyMasked} {
		    set portGuid [lindex [array get G data:PortGuid.$nodeGuid:*] 1]
		    set portAllreadyMasked 1
		}

		## Handle duplicate GUIDs - Case5.0: Duplicate port GUID which was never masked
		if {!$portAllreadyMasked && $bool_duplicatePortGuid} {
		    set prev_drPath $G(data:dr.path.to.guid.$portGuid)
		    if {![info exists DUPandZERO($portGuid,PortGUID)]} {
			lappend DUPandZERO($portGuid,PortGUID) $prev_drPath
		    }
		    lappend DUPandZERO($portGuid,PortGUID) $DirectPath
		    set currentMaskGuid [GetCurrentMaskGuid]
		    set MASK(PortGuid,$currentMaskGuid) $portGuid
		    lappend MASK(PortMask,$portGuid) $currentMaskGuid
		    set portGuid $currentMaskGuid
		    set bool_portGuidknown 0
		    AdvncedMaskGuid
		}

		## Handle duplicate GUIDs - Case6.0: Duplicate node GUID which was never masked
		if {!$nodeAllreadyMasked && $bool_duplicateNodeGuid} {
		    #set tmp_portGuid [lindex [array get G data:PortGuid.$nodeGuid:*] 1]
		    #set prev_drPath $G(data:dr.path.to.guid.$tmp_portGuid)
		    set prev_drPath $G(data:dr.path.to.node.$nodeGuid)

		    if {![info exists DUPandZERO($nodeGuid,NodeGUID)]} {
			lappend DUPandZERO($nodeGuid,NodeGUID) $prev_drPath
		    }
		    lappend DUPandZERO($nodeGuid,NodeGUID) $DirectPath
		    set currentMaskGuid [GetCurrentMaskGuid]
		    set MASK(NodeGuid,$currentMaskGuid) $nodeGuid
		    lappend MASK(NodeMask,$nodeGuid) $currentMaskGuid
		    set nodeGuid $currentMaskGuid
		    set bool_nodeGuidknown 0
		    AdvncedMaskGuid
		}
	    }
	} else {
	    # Note: if bool_skipGuids then assume that known GUIDs are legit
	    if {$bool_nodeGuidknown || $bool_portGuidknown} {
		#DZ: No need for that :continue
	    }
	}

	set G(data:guid.by.dr.path.$DirectPath) $portGuid
	### DiscoverFabric - Checks list
	## Checks list - Test1.0: Check if the new link is allready marked  -
	# if so removed $DirectPath.
	# Happens in switch systems and when a switch connects to himself
	if {![SetNeighbor $DirectPath $nodeGuid $entryPort]} {
	    set bool_badPathFound 1
	    continue;
	}

	## Checks list - Test2.0: Get nodeType
	if {[catch {set nodeType [GetParamValue Type $DirectPath]}]} {
	    set bool_badPathFound 1
	    continue;
	}

	## Checks list - Test3.0: Determine if you have reached an allready visited Switch
	if {($bool_nodeGuidknown) && ($nodeType == "SW")} {
	    continue;
	}

	## Checks list - Step1.0: The next line makes sure we only count the unknown Nodes
	if {!(($bool_nodeGuidknown) && ($nodeType == "CA"))} {
	    incr G(data:counter.$nodeType)
	}

	if {![info exists G(data:dr.path.to.guid.$portGuid)]} {
	    set G(data:dr.path.to.guid.$portGuid) $DirectPath
	}

	if {![info exists G(data:dr.path.to.node.$nodeGuid)]} {
	    set G(data:dr.path.to.node.$nodeGuid) $DirectPath
	}

	if {!$bool_nodeGuidknown} {
	    lappend G(data:list.node.guids)  $nodeGuid
	}
	if {!$bool_portGuidknown} {
	    lappend G(data:list.port.guids)  $portGuid
	}

	set G(data:NodeGuid.$portGuid) $nodeGuid
	set G(data:NodeInfo.$nodeGuid) $nodeInfo
	set G(data:PortGuid.$nodeGuid:$entryPort) $portGuid

	# Update Neighbor entry in the Neighbor Array.
	# it's possible it was allready updated in the "return to switch check"
	if {[llength $DirectPath] > 0} {
	    SetNeighbor $DirectPath $nodeGuid $entryPort
	}
	if {[catch {set tmp_nodeDesc [SmMadGetByDr NodeDesc -description "$DirectPath"]}]} {
	    set G(data:NodeDesc.$nodeGuid) "UNKNOWN"
	} else {
	    set G(data:NodeDesc.$nodeGuid) $tmp_nodeDesc
	}

	### DiscoverFabric - Build Ports List
	if { $nodeType == "CA" } {
	    ## Build Ports List - Case1.0: a HCA
	    set PortsList $entryPort
	} else {
	    ## Build Ports List - Case1.0: a SWITCH
	    if {[catch {set Ports [GetParamValue Ports $DirectPath]}]} {
		set bool_badPathFound 1
		continue;
	    }
	    set PortsList ""
	    for { set port 0 } { $port <= $Ports } { incr port } {
		lappend PortsList $port
	    }
	}

	# Gather SystemGUID,LID information into DUPandZERO Array
	set endLoop 0
	foreach ID "SystemGUID LID" {
	    if {[catch {set value [GetParamValue $ID $DirectPath -port 0]}]} {
		set bool_badPathFound 1
		set endLoop 1
		break;
	    } else {
		lappend DUPandZERO($value,$ID) "$DirectPath"
	    }
	}
	if {$endLoop} {continue;}

	### DiscoverFabric - SM handling
	# Check if the device is an SM and update portInfo
	set endLoop 0
	foreach port $PortsList {
	    if {[catch {set tmp_PortInfo [SmMadGetByDr PortInfo dump "$DirectPath" $port]}]} {
		set endLoop 1
		set bool_badPathFound 1
		continue;
	    }
	    if { ($nodeType == "CA") || (($nodeType == "SW") && ($port == 0) )} {
		set tmp_capabilityMask [GetWordAfterFlag $tmp_PortInfo -capability_mask]
		if {[expr 2 & $tmp_capabilityMask]} {
		    if {[catch {set tmp_LID [GetParamValue LID $DirectPath -port $port]}]} {
			set bool_badPathFound 1
			set endLoop 1
			continue;
		    }
		    if {![catch {set tmp_SMInfo [SmMadGetByLid SMInfo dump $tmp_LID ]}]} {
			set tmp_priState 0x[format %x [GetWordAfterFlag $tmp_SMInfo -pri_state]]
			lappend SM([expr $tmp_priState % 0x10]) "{$DirectPath} [expr $tmp_priState / 0x10]"
		    }
		}
	    }

	    set G(data:PortInfo.$nodeGuid:$port) $tmp_PortInfo

	    # The loop for non-switch devices ends here.
	    # This is also an optimization for switches ..
	    if { ( ($index_dr != 1) && ($port == $entryPort) ) || ($port == 0) } {
		continue;
	    }

	    if {[llength $DirectPath] > 1} {
		set tmp_revDrPath $G(data:rev.dr.path.[lrange $DirectPath 0 end-1])
		set G(data:rev.dr.path.$DirectPath) "$entryPort $tmp_revDrPath"
	    } else {
		set G(data:rev.dr.path.$DirectPath) $entryPort
	    }

	    # Check again that the local port is not down / ignore all other
	    # down ports
	    if {[catch {set tmp_log [GetParamValue LOG $DirectPath -port $port]}]} {
		set bool_badPathFound 1
		set endLoop 1
		break;
	    }
	    switch -- $tmp_log {
		"DWN" {
		    if { $index_dr == 1 } {
			inform "-E-localPort:local.port.down" -port $port
		    }
		    continue;
		}
		"INI" {
		    lappend G(data:list.logical.state) [join "$DirectPath $port"]
		}
	    }

	    ### DiscoverFabric - Add new dr path
	    # "$DirectPath $port" is added to the DirectPath list only if the
	    # device is a switch (or the root HCA), the link at $port is not
	    # DOWN, $port is not 0 and not the entry port
	    lappend G(data:list.direct.path) [join "$DirectPath $port"]
	}
	if {$endLoop} {continue;}
    }

    ### DiscoverFabric - Handle bad paths
    if {$bool_badPathFound} {
	lappend G(data:list.bad.paths) $DirectPath
	RemoveDirectPath $DirectPath
    }

    ### DiscoverFabric - Handle hidden fabric discovery
    if {$G(bool:hidden.fabric.discovered) == 0} {
	catch {set tmp_HiddenFabric [DiscoverHiddenFabric]}
	inform "-I-discover:discovery.status" -log
	inform "-I-exit:\\r"
	inform "-V-discover:end.discovery.header"
    }
    if {[info exists tmp_HiddenFabric] } {
	if {$tmp_HiddenFabric != 0} {
	    inform "-I-discover:discovery.status" -log
	    inform "-I-exit:\\r"
	    inform "-V-discover:end.discovery.header"
	}
    }
    return 0
}
##############################

### DZ: UP TO HERE DOCUMENTED

##############################
#  SYNOPSIS
#  DiscoverPath Path2Start node
#  FUNCTION
#  Traverses a path between two fabric nodes, reading info regarding the
#  nodes passed, and writing this data into various databases.
#  This procedure is used whenever a tool should traverse some path:
#  ibdiagpath, ibcfg, ibping,ibmad.
#  The path is defined by its two end points: the source node and the
#  destination node. The path between the endpoints will be a direct route
#  - in case of by-direct-path-addressing - or lid-route in case of by-name
#  or by-lid addressing.
#  INPUTS
#  $Path2Start is a direct route to the source node. In case of lid routing
#  it will be a part of the lid route to the destination node.
#  $node points to the destination is specified - either by a Direct route
#  (by-direct-path-addressing), or by LID (by-lid or by-name addressing).
#  OUTPUT
#  A direct route starting at the start (local or source) node
#       ending at the end node (destination or source )
#  DATAMODEL
#  Similarly to DiscoverFabric, the prod uses and updates these arrays
#  G(data:NodeInfo.<NodeGuid>)
#  G(data:list.direct.path)
#  Additionally, the following database is maintained:
#  G(DrPath2LID,<DirectPath>):
#     the LID of the entry port at the end of <DirectPath>
#  Also used: G(argv:* ) (= the parsed command line arguments) is used to
#  specify the addressing mode; and the pointer to the merged fabric
#  G(IBfabric:merged) (= the ibdm merging of topo file and .lst file)
proc DiscoverPath { _path2Start node } {
    global G errorCode errorInfo PATH_DUMP

    set bool_ignoreInit 0
    set bool_badPathFound 0
    set tmp_drPath ""
    if {$G(argv:symmetric.multi.processing) || ($G(var:tool.name) == "ibcfg") } {
	set bool_ignoreInit 1
    }

    if {$G(bool:topology.matched)} {
	set nameLast ""
    } else {
	set nameLast "-nameLast"
    }
    if {[set byDrPath [info exists G(argv:direct.route)]]} {
	set Path2End [join $node]
    } else {
	set destinationLid $node
	set blockNum [expr $destinationLid / 64]
	set LidMod64 [expr $destinationLid % 64]
    }

    # If the source node is a remote HCA, if I don't do the following
    # then my MADs will get stuck upon "entering-and-exiting" this node
    if {[catch {set sourceNodeType [GetParamValue Type $_path2Start]}]} {
	DumpBadLinks
	catch { close $G(logFileID) }
	inform "-E-ibdiagpath:route.failed" -DirectPath $tmp_drPath -port [GetEntryPort $_path2Start]
    }

    if { ($_path2Start != "") && $sourceNodeType != "SW" } {
	set Path2Start [lreplace $_path2Start end end]
    } else {
	set Path2Start $_path2Start
    }

    set DirectPath $Path2Start
    while { 1 } {
	if {[catch {set NodeInfo [SmMadGetByDr NodeInfo dump "$DirectPath"]}]} {
	    break;
	}
	if {([PathIsBad $DirectPath] > 1) || $bool_badPathFound} {
	    break;
	}

	set nodeGuid  [GetWordAfterFlag $NodeInfo "-node_guid"]
	set portGuid  [GetWordAfterFlag $NodeInfo "-port_guid"]
	set entryPort [GetEntryPort $DirectPath -byNodeInfo $NodeInfo]
	set G(data:guid.by.dr.path.$DirectPath) $portGuid
	set G(data:dr.path.to.guid.$portGuid) $DirectPath
	set G(data:NodeGuid.$portGuid) $nodeGuid
	set G(data:NodeInfo.$nodeGuid) $NodeInfo
	set DirectPath [join $DirectPath]

	if {$DirectPath != ""} {
	    if {![catch {set tmp_log [GetParamValue LOG [lrange $DirectPath 0 end-1] -port [lindex $DirectPath end] -byDr]}]} {
		if {$tmp_log == "INI"} {
		    lappend G(data:list.links.not.active.logical.state) [join "$DirectPath"]
		    if { !$bool_ignoreInit} {
			PrintPath $_path2Start
			inform "-E-ibdiagpath:link.not.active" -DirectPath0 [lrange $DirectPath 0 end-1] -port  [lindex $DirectPath end]
		    }
		}
	    }
	    if {![catch {set tmp_log [GetParamValue LOG $DirectPath -port $entryPort -byDr]}]} {
		if {$tmp_log == "INI"} {
		    lappend G(data:list.links.not.active.logical.state) [join "$DirectPath"]
		    if { !$bool_ignoreInit} {
			PrintPath $_path2Start
			inform "-E-ibdiagpath:link.not.active" -DirectPath0 $DirectPath -port $entryPort
		    }
		}
	    }
	}

	if {[llength $DirectPath] > 0} {
	    SetNeighbor $DirectPath $nodeGuid $entryPort
	}

	if {[catch {set tmp_nodeDesc [SmMadGetByDr NodeDesc -description "$DirectPath"]}]} {
	    set G(data:NodeDesc.$nodeGuid) "UNKNOWN"
	} else {
	    set G(data:NodeDesc.$nodeGuid) $tmp_nodeDesc
	}
	if { ![BoolWordInList $DirectPath $G(data:list.direct.path)] } {
	    lappend G(data:list.direct.path) $DirectPath
	}

	if {[catch {set nodeType [GetParamValue Type $DirectPath]}]} {
	    set bool_badPathFound 1
	    break;
	}
	if {[catch {set nodePorts [GetParamValue Ports $DirectPath]}]} {
	    set bool_badPathFound 1
	    break;
	}
	if {[catch {set nodeLid [GetParamValue LID $DirectPath -port $entryPort]}]} {
	    set bool_badPathFound 1
	    break;
	}

	if { $nodeLid == "0x" } { break; }

	if { $nodeType == "SW" } {
	    set G(DrPath2LID,$DirectPath:0) $nodeLid
	} else {
	    set G(DrPath2LID,$DirectPath:$entryPort) $nodeLid
	}
	if { $DirectPath != $Path2Start } {
	    set remoteLidGuidDev [DrPath2Name $DirectPath $nameLast -fullName -port $entryPort]
	    set PATH_DUMP($DirectPath,To) "$remoteLidGuidDev"
	    lappend PATH_DUMP(list,DirectPath) "$DirectPath,To"
	    #inform "-I-ibdiagpath:read.lft.to" "$localLidGuidDev"
	}
	############################################################
	### If we "discover" by means of direct route
	if {$byDrPath} {
	    # This is the stopping condition for direct routing
	    if { $DirectPath == $Path2End } { break; }
	    set exitPort [lindex $Path2End [llength $DirectPath]]

	    # if the user gives a direct path passing through a HCA
	    if { ( $nodeType != "SW" ) && ( $DirectPath != $Path2Start ) } {
		PrintPath $_path2Start
		inform "-E-ibdiagpath:direct.route.deadend" \
		    -DirectPath "$DirectPath"
	    }

	    # if port number is wrong (it exceeds the node's number of ports)
	    if { $exitPort > $nodePorts } {
		PrintPath $_path2Start
		inform "-E-ibdiagpath:direct.path.no.such.port" \
		    -DirectPath "$DirectPath" -port $exitPort
	    }
	    ############################################################
	} else {
	    ############################################################
	    # If we discover by means of lid-route

	    # This is the good stopping condition for lid routing
	    if { $nodeLid == $destinationLid } { break; }
	    # If we reached LID 0
	    if { $nodeLid == 0 } {
		PrintPath $_path2Start
		inform "-E-ibdiagpath:reached.lid.0" -DirectPath "$DirectPath"
	    }

	    # If we reached a HCA
	    if { ( $nodeType != "SW" ) && ( $DirectPath != $Path2Start ) } {
		PrintPath $_path2Start
		inform "-E-ibdiagpath:lid.route.deadend.reached.hca" \
		    -DirectPath [lrange $DirectPath 0 end-1] -lid $destinationLid -port [lindex $DirectPath 0]
	    }

	    # If we returned to an already-visited node: we are in a lid-loop -> exit
	    if { [info exists Guid2DrPath($nodeGuid)] } {
		PrintPath $_path2Start
		inform "-E-ibdiagpath:lid.route.loop" \
		    -DirectPath "$Guid2DrPath($nodeGuid)" -lid $destinationLid
	    } else {
		set Guid2DrPath($nodeGuid) $DirectPath
	    }

	    if { $nodeType != "SW" } {
		set exitPort $entryPort
	    } else {
		if {[catch {set FDBsBlock [SmMadGetByDr LftBlock dump "$DirectPath" $blockNum]}]} {
		    if { $errorCode == 0x801c } {
			PrintPath $_path2Start
			inform "-E-ibdiagpath:fdb.block.unreachable" \
			    -errorcode $errorCode -command "$cmd"
		    }
		    break;
		}
		if {[PathIsBad $DirectPath] > 1} { break; }
		set exitPort [expr [lindex $FDBsBlock $LidMod64]]
		if { ($exitPort == "0x00") } {
		    PrintPath $_path2Start
		    inform "-E-ibdiagpath:lid.route.deadend" \
			-DirectPath [lrange $DirectPath 0 end] -lid $destinationLid -port [lindex $DirectPath 0]
		}

		if { ($exitPort == "0xff")} {
		    PrintPath $_path2Start
		    inform "-E-ibdiagpath:fdb.bad.value" \
			-lid $destinationLid \
			-command "smLftBlockMad getByDr \{$DirectPath\} $blockNum" \
			-entry "\#$LidMod64" -value $exitPort
		}
	    }
	}

	if {[catch {set tmp_log [GetParamValue LOG $DirectPath -port $exitPort]}]} {
	    set bool_badPathFound 1
	    break;
	}

	if { $tmp_log == "DWN" } {
	    PrintPath $_path2Start
	    inform "-E-ibdiagpath:link.down" \
		-DirectPath "$DirectPath" -port $exitPort
	}
	set DirectPath [join "$DirectPath $exitPort"]

	# Note that lidGuidDev are corresponding to the "old" DirectPath
	# replace here the port number of the current device
	set tmp_drPath [lrange $DirectPath 0 end-1]
	set tmp_port $exitPort
	set localLidGuidDev  [DrPath2Name $tmp_drPath $nameLast -fullName -port $tmp_port]
	set PATH_DUMP($DirectPath,From) "$localLidGuidDev"
	lappend PATH_DUMP(list,DirectPath) "$DirectPath,From"
	#    inform "-I-ibdiagpath:read.lft.from" "$localLidGuidDev"
	############################################################
    }

    PrintPath $_path2Start
    if {([PathIsBad $DirectPath] > 1) || $bool_badPathFound} {
	DumpBadLinks
	catch { close $G(logFileID) }
	inform "-E-ibdiagpath:route.failed" -DirectPath $tmp_drPath -port $tmp_port
    }
    return [list $DirectPath]
}

proc PrintPath {_Path2Start args} {
    global PATH_DUMP G
    set bool_ignoreFirst [BoolWordInList bool_ignoreFirst $args]
    if {$G(var:tool.name) == "ibcfg"} {
	return -1
    }

    if {![info exists PATH_DUMP]} {
	return -1
    }

    if { ($_Path2Start != "") && [GetParamValue Type $_Path2Start] != "SW" } {
	set lidGuidDev $PATH_DUMP($_Path2Start,To)
	inform "-I-ibdiagpath:read.lft.from" "$lidGuidDev"
	set lidGuidDev $PATH_DUMP($_Path2Start,From)
	inform "-I-ibdiagpath:read.lft.to" "$lidGuidDev"
    }

    foreach drPath $PATH_DUMP(list,DirectPath) {
	set DirectPath [join [lrange [split $drPath ,] 0 end-1]]
	lappend listOfNames \"[lindex $PATH_DUMP($drPath) 0]\"
    }

    set max_nameLength [GetLengthMaxWord $listOfNames]

    if {$G(bool:topology.matched)} {
	foreach drPath $PATH_DUMP(list,DirectPath) {
	    set lidGuidDev $PATH_DUMP($drPath)
	    set lidGuidDev [join [lreplace $lidGuidDev 0 0 [AddSpaces \"[lindex $lidGuidDev 0]\" $max_nameLength]]]
	    set PATH_DUMP($drPath) $lidGuidDev
	}
    }

    foreach drPath $PATH_DUMP(list,DirectPath) {
	set tmp_drPath [join [lrange [split $drPath ,] 0 end-1]]
	if {[llength $tmp_drPath] <= [llength $_Path2Start]} {
	    continue
	}
	set lidGuidDev $PATH_DUMP($drPath)
	if {[lindex [split $drPath ,] end] == "From"} {
	    if {$bool_ignoreFirst} {
		set bool_ignoreFirst 0
	    } else {
		inform "-I-ibdiagpath:read.lft.from" "$lidGuidDev"
	    }
	} else {
	    inform "-I-ibdiagpath:read.lft.to" "$lidGuidDev"
	}
    }
    return 0
}
##############################

##############################
#  SYNOPSIS  DiscoverHiddenFabric
#  FUNCTION  Call the second run of discovery, this time for all the bad links
#  INPUTS    NULL
#  OUTPUT    0 if the DiscoverFabric method wasn't called, 1 if it did
proc DiscoverHiddenFabric {} {
    global G
    if {![info exists G(data:list.bad.paths)]} {
	return 1
    }
    set startIndex [llength $G(data:list.direct.path)]
    foreach badPath $G(data:list.bad.paths) {
	lappend G(data:list.direct.path) $badPath
    }
    set G(bool:hidden.fabric.discovered) 1
    DiscoverFabric 1 $startIndex
    return 0
}
##############################

##############################
#  SYNOPSIS     SetNeighbor
#  FUNCTION
#  setting the Neighbor info on the two end node
#  INPUTS
#  _directPath _nodeGuid _entryPort
#  OUTPUT
#       return 0/1 if Neighbor exists/not exists (resp.)
#  DATAMODEL
#   Neighbor(<NodeGuid>:<PN>)
proc SetNeighbor {_directPath _nodeGuid _entryPort} {
    global G Neighbor
    set prev_drPath [lrange $_directPath 0 end-1]
    if {![llength $_directPath] } {
	return 1
    }
    if {![info exists G(data:guid.by.dr.path.$prev_drPath)]} {
	return 1
    }
    set tmp_remoteSidePortGuid $G(data:guid.by.dr.path.$prev_drPath)
    if {![info exists G(data:NodeGuid.$tmp_remoteSidePortGuid)]} {
	return 1
    }
    set tmp_RemoteSideNodeGuid $G(data:NodeGuid.$tmp_remoteSidePortGuid)
    if {[info exists Neighbor($_nodeGuid:$_entryPort)]} {
	return 0
    }
    if {[info exists Neighbor($tmp_RemoteSideNodeGuid:[lindex $_directPath end])]} {
	return 0
    }
    set Neighbor($_nodeGuid:$_entryPort) "$tmp_RemoteSideNodeGuid:[lindex $_directPath end]"
    set Neighbor($tmp_RemoteSideNodeGuid:[lindex $_directPath end]) "$_nodeGuid:$_entryPort"
    return 1
}
##############################

proc Bool_SameDevice {_dr1 _dr2} {
    global G
    if {![info exists G(data:rev.dr.path.$_dr2)]} {
	return 0
    }
    set tmp_revDr $G(data:rev.dr.path.$_dr2)

    set tmp_badLinksDetectionValue $G(bool:bad.links.detected)
    set G(bool:bad.links.detected) 0
    for {set i 0} {$i < [llength $tmp_revDr]} {incr i} {
	set tmp_drPath_1 "$_dr1 [lrange $tmp_revDr 0 $i]"
	set tmp_drPath_2 "$_dr2 [lrange $tmp_revDr 0 $i]"
	if {[catch {set tmp_nodeInfo_1 [SmMadGetByDr NodeInfo dump "$tmp_drPath_1"]}]} {
	    set G(bool:bad.links.detected) $tmp_badLinksDetectionValue
	    return -1
	} else {
	    if {[catch {set tmp_nodeInfo_2 [SmMadGetByDr NodeInfo dump "$tmp_drPath_2"]}]} {
		set G(bool:bad.links.detected) $tmp_badLinksDetectionValue
		return -1
	    }
	    if {$tmp_nodeInfo_1 != $tmp_nodeInfo_2} {
		set G(bool:bad.links.detected) $tmp_badLinksDetectionValue
		return 0
	    }
	}
    }
    set G(bool:bad.links.detected) $tmp_badLinksDetectionValue
    return 1
}

##############################
#  SYNOPSIS     Bool_DuplicateGuids
#  FUNCTION Check if a given Node carries a Dupilcate GUID.
#               Using the Neighbor DB to compare the old neighbors to
#               the given Node neighbors
#  INPUTS   the node nodeGUID, and the directPath to the node from the
#               local system node
#               up to $_checks neighbors are being matched
#  OUTPUT   1 for duplicate 0 for not
proc Bool_DuplicateGuids { _NodeGuid _DirectPath _prev_drPath {_checks 1}} {
    global Neighbor G
    set i 0
    set noResponseToMad 0
    # we can not DR out of HCA so we can return 1 anyway
    ## If Checking a HCA, one cannot enter and exit the HCA,
    ### So instead we will run the smNodeInfoMad on the partiel Dr.
    switch -- [Bool_SameDevice $_DirectPath $_prev_drPath ] {
	"-1" -
	0 {return 1}
	1 {return 0}
    }

    foreach name [array names Neighbor $_NodeGuid:*] {
	if {$i >= $_checks} { break; }
	incr i
	if {[regexp {0x[0-9a-fA-F]+:([0-9]+)} $name all PN]} {
	    lappend portList $PN

	    #Found A port that once wasn't down and now it is DWN
	    #if { [GetParamValue LOG $_DirectPath -port $PN] == "DWN"} { return 1 }

	    #All known exits return error = it's not the same node
	    # we use SmMadGetByDrNo without detection for bad links, because we assume the direct path
	    # exists (in order to compare its endNode with the known node endNode)
	    # and the link is ACTIVE, its not have to be so no need fr setting that dr
	    # as Bad link also.
	    set tmp_badLinksDetectionValue $G(bool:bad.links.detected)
	    set G(bool:bad.links.detected) 0
	    if {[catch {set NodeInfo [SmMadGetByDr NodeInfo dump "$_DirectPath $PN"]}]} {
		set G(bool:bad.links.detected) $tmp_badLinksDetectionValue
		incr noResponseToMad
		continue;
	    }
	    set G(bool:bad.links.detected) $tmp_badLinksDetectionValue
	    set NodeGuid [GetWordAfterFlag $NodeInfo "-node_guid"]
	    set EntryPort [GetEntryPort "$_DirectPath $PN" -byNodeInfo $NodeInfo]
	    scan [split $Neighbor($name) :] {%s %s} nodeGuid entryPort
	    set nodeGuid [GetRealPort $nodeGuid]
	    if {"$nodeGuid:$entryPort" != "$NodeGuid:$EntryPort"} {
		return 1
	    }
	}
    }
    # if all the checks ended up with no response - we assume it's
    # not the same node
    if {$i == $noResponseToMad} {
	return 1
    }
    return 0
}

##############################
#  SYNOPSIS     DumpBadLidsGuids
#  FUNCTION Dump the retrived info during discovery, regarding
#               Duplicate Guids and lids, and zero values
proc DumpBadLidsGuids { args } {
    global G DUPandZERO errorInfo
    set bool_informBadGuidsReport 1
    set bool_skipZeroGuids [CheckSkipStatus zero_guids]
    set bool_skipDupGuids [CheckSkipStatus dup_guids]

    ### Checking for zero and duplicate IDs
    foreach entry [lsort [array names DUPandZERO]] {
	regexp {^([^:]*),([^:]*)$} $entry all value ID
	if {($value == 0) && ($ID != "LID")} {
	    if {$bool_skipZeroGuids} {
		continue;
	    }
	} else {
	    if {$bool_skipDupGuids} {
		continue;
	    }
	}

	if { ( ( [llength $DUPandZERO($entry)]==1 ) || ( $ID=="SystemGUID" ) ) && ( $value != 0 ) } {
	    unset DUPandZERO($entry)
	    continue;
	}
	if {($ID == "NodeGUID") || ($ID == "PortGUID")} {
	    set G(Fatal.err.found) 1
	}
	foreach DirectPath $DUPandZERO($entry) {
	    if {[catch {set devType [GetParamValue Type $DirectPath]} e]} {
		continue;
	    }
	    if {$devType == "SW"} {
		lappend listOfNames \"[DrPath2Name $DirectPath]\"
	    } else {
		if {$DirectPath == ""} {
		    lappend listOfNames "$G(var:desc.local.dev) \"[DrPath2Name $DirectPath]\""
		} else {
		    lappend listOfNames \"[DrPath2Name $DirectPath]\"
		}
	    }
	}
    }

    inform "-I-ibdiagnet:bad.guids.header"
    if {![info exists listOfNames]} {
	if {$bool_skipZeroGuids || $bool_skipDupGuids} {
	    inform "-I-reporting:skip.set.no.report"
	} else {
	    inform "-I-ibdiagnet:no.bad.guids"
	}
	return 1
    }

    proc compareEntries {a b} {
	scan [split $a ,] {%s %s} value_0 type_0
	scan [split $b ,] {%s %s} value_1 type_1
	if {$type_0 == $type_1} {
	    return [expr ($value_0  > $value_1 )]
	}
	if {$type_0 == "SystemGUID"} { return 1}
	if {$type_1 == "SystemGUID"} { return 0}
	if {$type_0 == "LID"} { return 1}
	if {$type_1 == "LID"} { return 0}
	if {$type_0 == "NodeGUID" } {return 1}
	if {$type_1 == "NodeGUID" } {return 0}
	return 0
    }

    #### Alliming the report
    foreach entry [lsort -command compareEntries [array names DUPandZERO]] {
	regexp {^([^:]*),([^:]*)$} $entry all value ID
	# llength will be diffrent then 1 when duplicate guids acored
	if { ( ( [llength $DUPandZERO($entry)]==1 ) || ( $ID=="SystemGUID" ) ) \
		 && ( $value != 0 ) } {
	    continue;
	}
	set bool_informBadGuidsReport 0
	set idx 0
	set paramList ""
	foreach DirectPath $DUPandZERO($entry) {
	    append paramList " -DirectPath${idx} \{$DirectPath\}"
	    incr idx
	}
	set maxName_Port [GetLengthMaxWord $listOfNames]
	if {$maxName_Port < 3} {
	    set maxName_Port 0
	} else {
	    incr maxName_Port 4
	}
	# use eval on the next line because $paramList is a list
	if {[catch {eval inform "-E-discover:zero/duplicate.IDs.found" -ID $ID -value $value $paramList -maxName_Port $maxName_Port} e]} {
	    continue;;
	}
    }
    if {$bool_informBadGuidsReport} {
	inform "-I-ibdiagnet:no.bad.guids"
    }
    return 0
}
##############################
#  SYNOPSIS     DumpBadLinksLogic
#  FUNCTION Dump to information retrived during discovery regarding all the
#               the links which are in INI state
proc DumpBadLinksLogic {} {
    global G

    inform "-I-ibdiagnet:bad.link.logic.header"
    if {[CheckSkipStatus logical_state]} {
	inform "-I-reporting:skip.set.no.report"
	return 1
    }

    set bool_informBadLogicLinksReport 1
    if {[info exists G(data:list.links.not.active.logical.state)]} {
	foreach link $G(data:list.links.not.active.logical.state) {
	    if {[lrange $link 0 end-1] == ""} {
		lappend listOfNames "$G(var:desc.local.dev) \"[DrPath2Name [lrange $link 0 end-1] nameOnly ]]\""
	    } else {
		lappend listOfNames \"[DrPath2Name [lrange $link 0 end-1] nameOnly ]\"
	    }
	    lappend listOfNames \"[DrPath2Name $link nameOnly ]\"
	}

	foreach link $G(data:list.links.not.active.logical.state) {                                                   
	    if {[PathIsBad $link] > 1} {continue;}
	    set paramlist "-DirectPath0 \{[lrange $link 0 end-1]\} -DirectPath1 \{$link\}"
	    eval inform "-W-ibdiagnet:report.links.init.state" $paramlist -maxName [GetLengthMaxWord $listOfNames]
	    set bool_informBadLogicLinksReport 0
	}
    }
    if {$bool_informBadLogicLinksReport} {
	inform "-I-ibdiagnet:no.bad.link.logic"
    }
    return 0
}

##############################
#  SYNOPSIS     RereadLongPaths
#  FUNCTION Send $G(argv:count) MADs that don't wait for replies
#               and then read all performance counters
proc RereadLongPaths {} {
    # send $G(argv:count) MADs that don't wait for replies
    # and then read all performance counters
    ## Retrying discovery multiple times (according to the -c flag)

    global G
    # The initial value of count is set to 4, since every link is traversed at least 3 times:
    # 1 NodeInfo, 1 PortInfo (once for every port), 1 NodeDesc
    set init_madsPerLink 4
    if { $init_madsPerLink > $G(argv:count) } {
	return 1
    }
    inform "-V-discover:long.paths"
    foreach DirectPath [lrange $G(data:list.direct.path) 1 end] {
	# start from the second path in $G(data:list.direct.path), because the first is ""
	# For the retries we use only the longest paths
	if { [lsearch -regexp $G(data:list.direct.path) "^$DirectPath \[0-9\]"] == -1 } {
	    for { set count $init_madsPerLink } { $count <= $G(argv:count) } { incr count } {
		if {[PathIsBad $DirectPath]} { break; }
		if {[catch { SmMadGetByDr NodeDesc dump "$DirectPath"}]} { break; }
	    }
	}
    }
    return 0
}
##############################

##############################
#  SYNOPSIS     PMCounterQuery
#  FUNCTION Query all known ports, then Send $G(argv:count) MADs that don't wait for replies
#               and then read all performance counters again
proc PMCounterQuery {} {
    global G PM_DUMP

    inform "-V-discover:long.paths"
    inform "-I-ibdiagnet:pm.counter.report.header"

    if {[CheckSkipStatus pm]} {
	inform "-I-reporting:skip.set.no.report"
	return 1
    }

    set bool_informPMcounterReport 1

    # Inform that the local link is in init state
    if {[info exists G(data:list.links.not.active.logical.state)]} {
	if {[llength [lindex $G(data:list.links.not.active.logical.state) 0]] == 1 } {
	    inform "-W-ibdiagnet:local.link.in.init.state"
	    RereadLongPaths
	    return 2
	}
    } else {
	set G(data:list.links.not.active.logical.state) ""
    }
    foreach directPath [lrange $G(data:list.direct.path) 0 end] {
	# start from the second path in $G(data:list.direct.path), because the first is ""
	# Ignore those links which has state INIT
	set bool_drPathIsInit 0
	set entryPort [GetEntryPort $directPath]

	if {[PathIsBad $directPath] > 1} { continue; }
	for {set i 0} {$i < [llength $directPath]} {incr i} {
	    if {[lsearch $G(data:list.links.not.active.logical.state) [lrange $directPath 0 $i]] != -1} {
		set bool_drPathIsInit 1
		break;
	    }
	}
	if {$bool_drPathIsInit} {continue;}
	if {[info exists tmp_LidPort]} {
	    unset tmp_LidPort
	}
	# preparing database for reading PMs
	if {![catch {set tmp_lid [GetParamValue LID $directPath -port $entryPort]}]} {
	    if { $tmp_lid != 0 } {
		if {[info exists G(argv:reset.performance.monitors)]} {
		    catch {pmClrAllCounters $tmp_lid $entryPort}
		}
		set tmp_LidPort "$tmp_lid:$entryPort"
                set LidPort($tmp_LidPort) $directPath
	    }
	}
	# Initial reading of Performance Counters
	if {[info exists tmp_LidPort]} {
	    if {[catch { set OldValues($tmp_LidPort) [join [GetPmList $tmp_LidPort]] } e] } {
                inform "-E-ibdiagpath:pmGet.failed" [split $tmp_LidPort :]
	    }
	}
	set tmp_LidPort "DannyZarko"
	set entryPort [lindex $directPath end]
	set tmp_drPath [join [lreplace $directPath end end]]
	if {[llength $tmp_drPath] == 0} {
	    set tmp_drPath ""
	}
	unset tmp_LidPort
	if {![catch {set tmp_lid [GetParamValue LID $tmp_drPath -port $entryPort]}]} {
	    if { $tmp_lid != 0 } {
		if {[info exists G(argv:reset.performance.monitors)]} {
		    catch {pmClrAllCounters $tmp_lid $entryPort}
		}
		set tmp_LidPort "$tmp_lid:$entryPort"
		set LidPort($tmp_LidPort) $tmp_drPath
	    }
	}
	# Initial reading of Performance Counters
	if {[info exists tmp_LidPort]} {
	    if {[catch { set OldValues($tmp_LidPort) [join [GetPmList $tmp_LidPort]] } e] } {
		inform "-E-ibdiagpath:pmGet.failed" [split $tmp_LidPort :]
	    }
	}
    }
    RereadLongPaths
    foreach lidPort [array names LidPort] {
	if {![info exists OldValues($lidPort)]} {continue;}

	set entryPort [lindex [split $lidPort :] 1]
	set directPath $LidPort($lidPort)
	set name [DrPath2Name $directPath -fullName -port $entryPort]

	# Final reading of Performance Counters
	if [catch { set NewValues($lidPort) [join [GetPmList $lidPort]] }] {
	    inform "-E-ibdiagpath:pmGet.failed" [split $lidPort :]
	}
	set pmList ""
	if {![info exists NewValues($lidPort)]} {continue;}
	for { set i 0 } { $i < [llength $NewValues($lidPort)] } { incr i 2 } {
	    set oldValue [lindex $OldValues($lidPort) [expr $i + 1]]
	    set newValue [lindex $NewValues($lidPort) [expr $i + 1]]
	    lappend pmList [expr $newValue - $oldValue]
	}

	inform "-V-ibdiagpath:pm.value" "$name $pmList"

	set badValues ""
	## -pm option
	# set a list of all pm counters and reduced each one which is reported as an error
	set pmCounterList [lrange $G(var:list.pm.counter) 0 end-1]

	foreach entry [ComparePMCounters $OldValues($lidPort) $NewValues($lidPort)] {
	    scan $entry {%s %s %s} parameter err value
	    switch -exact -- $err {
		"valueChange" {
		    regsub -- "->" $value "-" exp
		    unset oldValue
		    unset newValue
		    scan [split $exp -] {%s %s} oldValue newValue
		    if {![info exists newValue]} {
			set newValue $oldValue
			set oldValue 0
		    }
		    set diffValue [expr $newValue - $oldValue]
		    lappend badValues "$parameter=0x[format %x $newValue] \(Increase by $diffValue during $G(var:tool.name) scan.\)"
		}
		"overflow" {
		    lappend badValues "$parameter=$value \(overflow\)"
		}
		"exceeded"  {
		    set value 0x[format %x $value]
		    lappend badValues "$parameter=$value"
		}
	    }
	}
	if { $badValues != "" } {
	    set bool_informPMcounterReport 0
	    inform "-W-ibdiagnet:bad.pm.counter.report" -DirectPath0 $directPath -listOfErrors $badValues -port0 $entryPort
	}

	if {[info exists G(argv:performance.monitors)]} {
	    lappend PM_DUMP(nodeNames) $name
	    set PM_DUMP($name,pmCounterList) $pmCounterList
	    set PM_DUMP($name,pmCounterValue) $NewValues($lidPort)
	}
    }
    if {$bool_informPMcounterReport} {
	inform "-I-ibdiagnet:no.pm.counter.report"
    }
    if {[info exists G(argv:performance.monitors)]} {
	writePMFile
    }
    return 0
}


proc RunPkgProcs {} {
    global PKG_FLAGS G

    ### RunPkgProcs- Execute external pkg procs
    ## - Step1: Create DB and check validation
    ## Execute external pkg procs - Step1.0: If array / list of flags doesn't exists return 1
    if {![info exists PKG_FLAGS] || ![info exists PKG_FLAGS(data:list.flags)]} {
	return 1
    }
    ## Execute external pkg procs - Step1.1: Create list of execution procs
    set tmp_listOfProcs ""
    set listOfProcs [array names PKG_FLAGS -regexp "\[^.\]+.run"]
    ## Execute external pkg procs - Step1.2: Create list of exteranl flags
    set tmp_listOfFlags ""
    set listOfFlags $PKG_FLAGS(data:list.flags)
    ## Execute external pkg procs - Step1.3: If one list is empty, retun 1
    if {![llength $listOfProcs] || ![llength $listOfFlags]} {
	return 1
    }
    ## Execute external pkg procs - Step1.4: Create list of external flags provided (By user/default)
    foreach tmp_flag $listOfFlags {
	if {[info exists G(argv:[string trimleft $tmp_flag -])]} {
	    lappend tmp_listOfFlags $tmp_flag
	}
    }
    set listOfFlags $tmp_listOfFlags
    set tmp_listOfFlags ""
    ## Execute external pkg procs - Step1.4: Create list of external procs to run
    foreach tmp_flag $listOfFlags {
	if {[lsearch $listOfProcs data:${tmp_flag}.run] != -1} {
	    lappend tmp_listOfProcs $PKG_FLAGS(data:${tmp_flag}.run)
	    lappend tmp_listOfFlags $tmp_flag
	}
    }
    set listOfProcs $tmp_listOfProcs
    set listOfFlags $tmp_listOfFlags
    ### Now the list will match places flagName<->procName
    ## Execute external pkg procs - Step1.5: Sort procs by order of apperance
    # TODO

    ## Execute external pkg procs - Step2: Start execution for each remaining
    for {set i 0} {$i < [llength $listOfFlags]} {incr i} {
	set tmp_flag [lindex $listOfFlags $i]
	set tmp_val $G(argv:[string trimleft $tmp_flag -])
	regexp {^::[^:]+::([^ ]*)} $PKG_FLAGS(data:$tmp_flag.parse) . tmp_procParse
	if {$tmp_procParse != ""} {
	    if {[$PKG_FLAGS(data:$tmp_flag.parse) $tmp_val] != 0} {
		continue
	    }
	}
	set bool_firstReport 0
	inform "-I-ibdiagnet:external.flag.execute.header" -flag $tmp_flag
	set tmp_proc [lindex $listOfProcs $i]
	set bool_localPort 1
	foreach directPath $G(data:list.direct.path) {
	    if {[llength $directPath] == 0 } {
		set list_drPath_entryPort "$directPath:[GetEntryPort $directPath]"
	    } else {
		set list_drPath_entryPort [list [lrange $directPath 0 end-1]:[lindex $directPath end]]
		lappend list_drPath_entryPort $directPath:[GetEntryPort $directPath]
	    }
	    foreach drPath_entryPort $list_drPath_entryPort {
		regexp {^([^:]*):(.+)$} $drPath_entryPort . directPath entryPort
		if {($directPath == "") && !$bool_localPort} {
		    continue;
		} else {
		    set bool_localPort 0
		}
		## Execute external pkg procs - Step2.0: Create info to send to tmp_proc
		if {[PathIsBad $directPath] > 1 } {continue; }
		set tmp_portInfo [lstInfo port $directPath $entryPort]
		set res "-Type [lindex $tmp_portInfo 0]"

		foreach tmp_listEntry [lrange $tmp_portInfo 1 end]  {
		    catch {unset $field}
		    catch {unset $value}
		    scan [split $tmp_listEntry :] {%s %s} field value
		    if {(![info exists field]) || (![info exists value]) || !(([llength [split $tmp_listEntry :]] == 2) && ([llength $tmp_listEntry] == 1)) } {
			continue
		    }
		    lappend res -$field
		    lappend res $value
		}
		lappend res -name
		lappend res [DrPath2Name $directPath -nameLast -fullName -port $entryPort]
		lappend res -dr
		lappend res "$directPath"
		## Execute external pkg procs Step2.1: If current flag has check procedure run it.
		regexp {^::[^:]+::([^ ]*)} $PKG_FLAGS(data:$tmp_flag.check) . tmp_procCheck
		if {$tmp_procCheck != ""} {
		    if {[$PKG_FLAGS(data:$tmp_flag.check) $tmp_val $res] != 0} {
			continue
		    }
		}
		## Execute external pkg procs Step2.2: Execute flag run procedure
		$tmp_proc $tmp_val $res
	    }
	}
	## Execute external pkg procs Step2.3: If current flag has dump procedure run it.
	regexp {^::[^:]+::([^ ]*)} $PKG_FLAGS(data:$tmp_flag.dump) . tmp_procDump
	if {$tmp_procDump != ""} {
	    $PKG_FLAGS(data:$tmp_flag.dump) $tmp_val
	    set bool_firstReport 1
	}
	if {!$bool_firstReport} {
	    inform "-I-ibdiagnet:external.flag.execute.no.report"
	}
    }
    return 0
}
### DZ: UP TO HERE VERIFIED

#####################################################################
### GENERAL PURPOSE PROCs
######################################################################
##############################
#  SYNOPSIS BoolWordInList word list
#  FUNCTION Indicates whether $word is a word in $list
#  INPUTS   a string $word and a list $list#
#  OUTPUT   1 or 0 - if $word is or isn't a word in $list (resp.)
proc BoolWordInList { word list } {
    return [expr [lsearch -exact $list $word] >= 0 ]
}
##############################

##############################
#  SYNOPSIS RemoveElementFromList
#  FUNCTION remove an entry from a list
#  INPUTS   original list _word and the unrequierd entry _element
#  OUTPUT   the orignal list if if $_element isn't in $_list.
proc RemoveElementFromList {_list _element } {
    set tmpIndex [lsearch -exact $_list $_element]
    if {$tmpIndex == -1} { return $_list}
    return [lreplace $_list $tmpIndex $tmpIndex]
}
##############################


##############################
#  SYNOPSIS GetWordAfterFlag list flag
#  FUNCTION Returns the entry in $list that is right after $flag (if exists)
#  INPUTS   a list $list and a string $flag
#  OUTPUT   a srting, which is the word in $list which is right after $flag
#     - if exists - if not, the empty string is returned.
proc GetWordAfterFlag { list flag } {
    if {[set index [expr [lsearch -exac $list $flag] +1]]} {
	return [lindex $list $index]
    }
    return ""
}
##############################

##############################
#  SYNOPSIS Bar char length
#  FUNCTION Return a string made of $length times $char
#  INPUTS   a string $char and an integer $length
#  OUTPUT   a srting, which is made of $length times duplicates of $char
proc Bar { char length } {
    return [string repeat $char $length]
}
##############################

##############################
#  SYNOPSIS AddZeroes num length
#  FUNCTION adds zeroes to the LHS of $number for it to be of string length
#     $length
#  INPUTS   a number $num and an integer $length
#  OUTPUT   a srting, of length $length made of padding $num with zeroes on
#     the LHS.If the length of $num is greater than $length,
#     the procedure will return $num.
proc AddZeroes { num length } {
    return "[Bar 0 [expr $length - [string length $num]]]$num"
}
##############################

##############################
#  SYNOPSIS RemoveZeroes num
#  FUNCTION erase all zeroes at the LHS of $num. The number "0" returns "0"
#     (and not "")
#  INPUTS   an integer $length
#  OUTPUT   a number, that is made of erasing all zeroes at the LHS of $num.
#     If $num == 0, the procedure returns 0
proc RemoveZeroes { num } {
    regsub {^0*(.+)$} $num {\1} num
    return $num
}
##############################

##############################
#  SYNOPSIS Hex2Bin hex_list
#  FUNCTION turns a list of hexa munbers into a list of binary numbers
#  INPUTS   a list $list of hexadecimal numbers
#  OUTPUT   a list, which is made of the numbers of $list, represented in
#     Binary base.
proc Hex2Bin { hex_list } {
    set bin_list ""
    array set hexbit2bin {
	0 0000  1 0001  2 0010  3 0011
	4 0100  5 0101  6 0110  7 0111
	8 1000  9 1001  a 1010  b 1011
	c 1100  d 1101  e 1110  f 1111
    }
    foreach hex $hex_list {
	regsub {^0x} [string tolower $hex] {} hex
	set bin ""
	foreach hbit [split $hex ""] {
	    append bin $hexbit2bin($hbit)
	}
	lappend bin_list $bin
    }
    return $bin_list
}
##############################

##############################
#  SYNOPSIS  GetLengthMaxWord _list
#  FUNCTION  return the char length of the maximum word from the list
#  INPUTS    a list of words
#  OUTPUT    the length of te longest word
proc GetLengthMaxWord {_list} {
    set maxLength 0
    foreach field $_list {
	if {[string length $field] > $maxLength } {
	    set maxLength [string length $field]
	}
    }
    return $maxLength
}
##############################

##############################
#  SYNOPSIS AddSpaces _word  _desiredLength
#  FUNCTION addind requierd amount of spaces
#  INPUTS   the original word, and the amount of spaces to append to it
#  OUTPUT   word + spaces
proc AddSpaces {_word _desiredLength} {
    set wordLength [string length $_word]
    if {$wordLength >= $_desiredLength } {
	return $_word
    } else {
	return "$_word[string repeat " " [expr $_desiredLength - $wordLength]]"
    }
}
##############################

##############################
#  SYNOPSIS ProcName args (<- args may be a positive integer)
#  FUNCTION Return the name of the calling procedure
#  INPUTS   optinally - a positive integer
#  OUTPUT   the name of the calling procedure
#     (if $args != "" -> the name of the calling procedure $args levels up)
proc ProcName { args } {
    set upLevels 0
    if { $args != "" } { set upLevels $args }
    return [lindex [info level [expr [info level] -1 -$upLevels]] 0]
}

##############################
# get a list of numbers and generate a nice consolidated sub list
proc groupNumRanges {nums} {
    if {[llength $nums] <= 1} {
	return [lindex $nums 0]
    }

    set start -1
    set res ""
    if {[catch {set snums [lsort -dictionary $nums]}]} {
	set snums [lsort $nums]
    }
    set last [lrange $snums end end]
    set start [lindex $snums 0]
    set end $start
    foreach n $snums {
	if {([RemoveZeroes $n] > [RemoveZeroes $end] + 1)} {
	    if {$start == $end} {
		append res "$end,"
	    } else {
		append res "$start..$end,"
	    }
	    set start $n
	    set end $n
	} else {
	    set end $n
	}
    }
    if {$start == $end} {
	append res "$end,"
    } else {
	append res "$start..$end,"
    }
    return "\[[string range $res 0 end-1]\]"
}

# process every group by splitting it to pre num and post
# then look for matches and build next groups
proc groupingEngine {groups} {
    set res {}
    foreach group $groups {
	set idx [lindex $group 0]
	set word [lindex $group 1]

	# try to find a number on the right of the idx:
	set prefix [string range $word 0 [expr $idx -1]]
	set suffix [string range $word $idx end]
	if {![regexp {([^0-9]*)([0-9]+)(.*)} $suffix d1 w1 num w3]} {
	    # no number - just keep this group
	    lappend res $group
	    continue;
	}

	append prefix $w1
	set suffix $w3
	set key "$prefix $suffix"
	lappend NEW_GROUPS($key) $num
    }

    # go over all new groups and see if we can collapse them:
    foreach pNs [lsort [array names NEW_GROUPS]] {
	set ranges [groupNumRanges $NEW_GROUPS($pNs)]
	foreach range $ranges {
	    set prefix [lindex $pNs 0]
	    set suffix [lindex $pNs 1]
	    set gIndx [expr [string length $prefix] + [string length $range]]
	    lappend res "$gIndx $prefix$range$suffix"
	}
    }
    return $res
}

# Algorithm:
# split the given words on the first number
# then group on the leading string and sort for continoues
proc compressNames {words} {
    # we need to prepare the first stage which is a list of words and
    # simply the index 0 for were to start the search for integers
    foreach word $words {
	lappend groups [list 0 $word]
    }

    # now call the grouping engine
    set prevGroups 0
    while {$prevGroups != [llength $groups]} {
	set prevGroups [llength $groups]
	set groups [groupingEngine $groups]
    }

    set res ""
    foreach group $groups {
	lappend res [lindex $group 1]
    }
    return [join $res]
}
##############################

######################################################################
### Handling Duplicated Guids
######################################################################
##############################
#  SYNOPSIS  AdvncedMaskGuid
#  FUNCTION  advenced current mask guid by _increment(default set to 1)
#  INPUTS    _increment(default set to 1)
#  OUTPUT   0 when  _increment is not an integer, otherwise return 1
proc AdvncedMaskGuid { {_increment 1}} {
    #ASSUME MASK GUID FORMAT IS HARD CODED
    global MASK
    if {![string is integer $_increment]} {
	return 1
    }
    incr MASK(CurrentMaskGuid) $_increment
    return 0
}
##############################

##############################
#  SYNOPSIS GetCurrentMaskGuid
#  FUNCTION return the current mask guid
#  INPUTS   NULL
#  OUTPUT   mask guid
proc GetCurrentMaskGuid {} {
    global MASK
    set tmp $MASK(CurrentMaskGuid)
    set tmp [format %08x $tmp]
    set tmp "0xffffffff${tmp}"
    return $tmp
}
##############################


##############################
#  SYNOPSIS  BoolIsMaked { _currentMaskGuid}
#  FUNCTION  checks if a guid is masked
#  INPUTS    GUID
#  OUTPUT    0 or 1
proc BoolIsMaked { _currentMaskGuid} {
    return [string equal 0xffffffff [string range $_currentMaskGuid 0 9]]
}
##############################

##############################
#  SYNOPSIS  GetRealPort { _currentMaskGuid}
#  FUNCTION  return the masked guid
#  INPUTS    mask guid
#  OUTPUT    real guid
proc GetRealPort { _currentMaskGuid} {
    global MASK
    set tmpGuid $_currentMaskGuid
    while {[BoolIsMaked $tmpGuid]} {
	if {[info exists MASK(PortGuid,$tmpGuid)]} {
	    set tmpGuid $MASK(PortGuid,$tmpGuid)
	}
	if {[info exists MASK(NodeGuid,$tmpGuid)]} {
	    set tmpGuid $MASK(NodeGuid,$tmpGuid)
	}
    }
    return $tmpGuid
}

######################################################################
### Detecting bad links on a path on which a packet was lost
######################################################################

##############################
#  SYNOPSIS
#  PathIsBad path
#  FUNCTION
#  returns 1 if the direct path $path contains a link that was found to be
#  "bad"; it returns 0 otherwise.
#  INPUTS
#  $path - a direct path: a list of integers separated by spaces - denoting
#  a direct route in the fabric (= a list of exit ports, starting at the
#  fabric's source node).
#  OUTPUT
#  0/1 - according to whether $path has a bad link or not
#  DATAMODEL
#  the procedure uses the database G(bad,paths,<DirectPath>), which is set in
#  "DetectBadLinks", and that denotes the reasons (errors) why the link
#  at the end of DirectPath was found to be bad.
proc PathIsBad { path } {
    global G
    set boolOnlyBadPMs 1
    set boolPathIsBad  0
    for { set i 0 } { $i < [llength $path] } { incr i } {
	if { [info exists G(bad,paths,[lrange $path 0 $i])] } {
	    set boolPathIsBad 1
	    # ignore PMcounter types error
	    foreach arrayEntry $G(bad,paths,[lrange $path 0 $i]) {
		if {[lindex $arrayEntry 1] != "badPMs"} {
		    set boolOnlyBadPMs 0
		}
	    }
	}
    }
    if {$boolPathIsBad} {
	if {$boolOnlyBadPMs} {
	    return 1
	} else {
	    return 2
	}
    }
    return 0
}
##############################

##############################
#  SYNOPSIS
#  DetectBadLinks starting cgetCmd cmd args
#  FUNCTION
#  Explores the direct route on which $cmd failed, and detects bad link(s)
#  along this path.
#  The exploration algorithm:... TODO: fill this in ...
#  The bad are then written to the database $G(bad,links,*).
#  This procedure is called by "SmMadGetByDr", when the global variable
#  $G(bool:bad.links.detected) is set.
#  INPUTS
#  $status - the last exit status of $cmd
#  $cgetCmd - the command that returns the result of $cmd
#  $cmd - the name of the command that failed
#  $args - the arguments of $cmd
#  OUTPUT
#  the result data of running $cmd $args - if available;
#  if the data in not available, returns with error code 1.
#  DATAMODEL
#  $G(var:badpath.*) - a database used, which defines various
#   parameters for the bad paths exploration: limits and growth rate for
#   the retries of trying to run $cmd $args and error threshold to stop it.
#  $G(bad,paths,<DirectPath>) - a database used, that denotes the reasons
#   (errors) why the link at the end of DirectPath was found to be bad.
#  $InfoPm(<PM>) - the width-in-bits of each PM and its error threshold

proc DetectBadLinks { status cgetCmd cmd args } {
    global G env
    set args [join $args]
    set DirectPath [join [lindex $args 0]]
    set data -1
    if  {$status == 0}  { set data [eval $cgetCmd] }
    inform "-V-ibdiagnet:bad.links.detected" -path "$DirectPath"

    # setting retriesStart, retriesEnd, maxnErrors, retriesGrowth
    foreach entry [array names G var:badpath.*] {
	set name [lindex [split $entry .] end]
	set $name $G($entry)
    }

    inform "-V-ibdiagnet:incremental.bad.links" -path "$DirectPath"
    set errors 0
    for   { set maxnRetries $retriesStart } {( $maxnRetries <= $retriesEnd ) && ( $errors < $maxnErrors ) } { set maxnRetries [expr $maxnRetries * $retriesGrowth] } {
	for { set I 0 ; set errors 0 } { ($I < [llength $DirectPath]) && ($errors == 0) } { incr I } {
	    set ShortPath [lrange $DirectPath 0 $I]
	    set getCmd [concat "smNodeInfoMad getByDr [list $ShortPath]"]
	    if {[PathIsBad $ShortPath] > 1 } { break; }
	    for { set retry 0 } { ($retry < $maxnRetries) && ($errors < $maxnErrors) } { incr retry } {
		incr errors [expr [eval $getCmd] != 0]
	    }
	    if { ($ShortPath==$DirectPath) && ($retry>$errors)} {
		set data [eval $cgetCmd]
	    }
	}
    }

    # If errors count did not reach $maxnErrors, the path is considered to be OK
    if { $errors < $maxnErrors } { return $data }

    # If it did - the link at the end of ShortPath is "bad"
    if { [llength $ShortPath] <= 1 } {
	if { ( $retry == $errors ) } {
	    inform "-E-localPort:local.port.crashed" -command "$getCmd" -DirectPath0 ""
	} else {
	    inform "-E-localPort:local.port.failed" \
		-fails "$errors" -attempts $retry -command "$getCmd"
	}
    }

    if { ( $retry == $errors ) } {
	lappend G(bad,paths,$ShortPath) "-error noInfo -command \{$getCmd\}"
	inform "-V-badPathRegister" -error noInfo -command "$getCmd"
	return -code 1 -errorinfo "Direct Path \"$ShortPath\" is bad = noInfo"
    } else {
	lappend G(bad,paths,$ShortPath) "-error madsLost -ratio $errors:$retry -command \{$getCmd\}"
    }

    return $data
}
######################################################################

##############################
proc ComparePMCounters { oldValues newValues args } {
    global G
    array set InfoPm {
	port_select                     { -width 8  -thresh 0  }
	counter_select                  { -width 16 -thresh 0  }
	symbol_error_counter            { -width 16 -thresh 1  }
	link_error_recovery_counter     { -width 8  -thresh 1  }
	link_down_counter               { -width 8  -thresh 1  }
	port_rcv_errors                 { -width 16 -thresh 1  }
	port_rcv_remote_physical_errors { -width 16 -thresh 0  }
	port_rcv_switch_relay_errors    { -width 16 -thresh 0  }
	port_xmit_discard               { -width 16 -thresh 10 }
	port_xmit_constraint_errors     { -width 8  -thresh 10 }
	port_rcv_constraint_errors      { -width 8  -thresh 10 }
	local_link_integrity_errors     { -width 4  -thresh 10 }
	excesive_buffer_errors          { -width 4  -thresh 10 }
	vl15_dropped                    { -width 16 -thresh 10 }
	port_xmit_data                  { -width 32 -thresh 0  }
	port_rcv_data                   { -width 32 -thresh 0  }
	port_xmit_pkts                  { -width 32 -thresh 0  }
	port_rcv_pkts                   { -width 32 -thresh 0  }
    }

    set errList ""
    set pmRequestList ""
    if {[info exists G(argv:query.performance.monitors)]} {
	set pmRequestList [split $G(argv:query.performance.monitors) {, =}]
    }
    foreach parameter [array names InfoPm] {
	ParseOptionsList $InfoPm($parameter)
	if { ! [info exists cfg(thresh)] } { continue; }
	if { $cfg(thresh) == 0 } { continue; }

	set oldValue   [GetWordAfterFlag $oldValues $parameter]
	set newValue   [GetWordAfterFlag $newValues $parameter]
	set delta   [expr $newValue - $oldValue]
	set overflow   0x[Bar f [expr $cfg(width) / 4]]

	if { ( $delta >= $cfg(thresh) ) || ( $oldValue > $newValue ) } {
	    lappend errList "$parameter valueChange $oldValue->$newValue"
	} elseif { ( $oldValue == $overflow ) || ( $newValue == $overflow ) } {
	    lappend errList "$parameter overflow $overflow"
	} elseif {[info exists G(argv:query.performance.monitors)]} {
	    if {[lsearch $pmRequestList $parameter] != -1} {
		set pmTrash [GetWordAfterFlag $pmRequestList $parameter]
		if {$newValue >= $pmTrash} {
		    lappend errList "$parameter exceeded 0x[format %lx $newValue]"
		}
	    } elseif {[lsearch $pmRequestList "all"] != -1} {
		set pmTrash [GetWordAfterFlag $pmRequestList "all"]
		if {$newValue >= $pmTrash} {
		    lappend errList "$parameter exceeded 0x[format %lx $newValue]"
		}
	    }
	}
    }
    return $errList
}
##############################

##############################
#  SYNOPSIS
#  DumpBadLinks
#  FUNCTION
#  Pretty-printing of the bad links information
#  INPUTS
#  NULL
#  OUTPUT
#  Prints to the standard output the list of bad links, with proper
#  indentation, and - optionally - with infromation why this link was
#       found to be bad.
#  DATAMODEL
#  G(bad,paths,<DirectPath>) - a database used, that denotes the reasons
#   (errors) why the link at the end of DirectPath was found to be bad.
proc DumpBadLinks {} {
    global G

    if { ! [llength [array names G "bad,paths,*"]] } {
	inform "-I-ibdiagnet:no.bad.paths.header"
	return
    }
    inform "-I-ibdiagnet:bad.links.header"

    foreach entry [array names G "bad,paths,*"] {
	set DirectPath [lindex [split $entry ,] end]
	set linkNames "Link at the end of direct route \"[ArrangeDR $DirectPath]\""
	#set linkNames "Link at the end of direct route \{$DirectPath\}"
	if {[DrPath2Name $DirectPath] != ""} {
	    append linkNames " \"[DrPath2Name $DirectPath]\""
	}
	array set BadPathsLinksArray "\{$linkNames\} \{$G($entry)\}"
    }
    ### pretty-printing of a list of links
    set LinksList [array names BadPathsLinksArray]
    foreach item $LinksList {
	set link [lindex $item end]
	lappend llen [string length [lindex $link 0]] [string length [lindex $link 1]]
    }
    set maxLen1 [lindex [lsort -integer $llen] end]

    set space(0)   "   "
    set space(1)   [Bar " " [string length $space(0)]]
    array set prefix {
	"names:external" "Cable:"
	"names:internal" "Internal link:"
    }
    foreach kind [array names prefix] {
	lappend prefix_llen [string length $prefix($kind)]
    }
    foreach kind [array names prefix] {
	set maxLen0  [lindex [lsort -integer $prefix_llen] end]
	set rubberLen0  [expr $maxLen0 - [string length $prefix($kind)]]
	set space($kind) "$space(0) $prefix($kind)[Bar " " $rubberLen0]"
    }
    array set sym {
	names:external,conn  "="
	names:external,cable "-"
	names:internal,conn  "."
	names:internal,cable "."
    }

    foreach item $LinksList {
	set kind [lindex $item 0]
	set link [lsort -dictionary [lindex $item end]]
	if { [llength $link] == 1 } { lappend link "? (unknown port)" }
	if { ! [regexp {[^ ]} [lindex $link 0]] } {
	    set link [list [lindex $link 1] "? (unknown port)"]
	}

	if { ! [info exists prefix($kind)] } {
	    lappend portsList "Z.$item"
	    set stdoutList(Z.$item) "$space(0) $item"
	    set stdoutErrs(Z.$item) $BadPathsLinksArray($item)
	} else {
	    regsub {^[^\(]*\((.*)\)$} [lindex $link 0] {\1} p0
	    lappend portsList $p0
	    set rubberLen1 [expr $maxLen1 - [string length [lindex $link 0]] + 3]
	    set cable "$sym($kind,conn)[Bar $sym($kind,cable) $rubberLen1]$sym($kind,conn)"
	    set stdoutList($p0) "$space($kind) [lindex $link 0] $cable [lindex $link 1]"
	    set stdoutErrs($p0) $BadPathsLinksArray($item)
	}
    }

    set line ""
    foreach item [lsort -dictionary $portsList] {
	if { $line != [set line $stdoutList($item)] } {
	    inform "-I-ibdiagnet:bad.link" -link "$line"
	    inform "-I-ibdiagnet:bad.link.errors" \
		-errors " $space(1) Errors:\n $space(1)  [join $stdoutErrs($item) "\n $space(1)  "]"
	}
    }
    inform "-I-ibdiagnet:bad.links.err.types"
    return 0
}

##############################
#  SYNOPSIS     RemoveDirectPath
#  FUNCTION Removes a direct path from $G(data:list.direct.path)
#               - when we in a loop
#               - when we enter an allready known switch
#               - when we returned on an old link from the other end
proc RemoveDirectPath {_drPath } {
    global G
    set tmpList $G(data:list.direct.path)
    set tmpList [RemoveElementFromList $G(data:list.direct.path) $_drPath ]
    set G(data:list.direct.path) $tmpList
    if {[info exists G(data:guid.by.dr.path.$_drPath)]} {
	unset G(data:guid.by.dr.path.$_drPath)
    }
    return 0
}
##############################

######################################################################
### SM handling
######################################################################
proc CheckSM {} {
    global SM G
    set master 3
    if {![info exists SM($master)]} {
	inform "-I-ibdiagnet:bad.sm.header"
	inform "-E-ibdiagnet:no.SM"
    } else {
	if {[llength $SM($master)] != 1} {
	    inform "-I-ibdiagnet:bad.sm.header"
	    inform "-E-ibdiagnet:many.SM.master"
	    foreach element $SM($master) {
		set tmpDirectPath [lindex $element 0]
		set nodeName [DrPath2Name $tmpDirectPath -port [GetEntryPort $tmpDirectPath]]
		if { $tmpDirectPath == "" } {
		    set nodeName "$G(var:desc.local.dev) : $nodeName"
		}
		inform "-I-ibdiagnet:SM.report.body" $nodeName [lindex $element 1]
	    }
	}
    }
    return 0
}

proc DumpSMReport { {_fileName stdout} }  {
    global SM G
    set tmpStateList "not-active dicovering standby master"
    for {set i 3} {$i > -1} {incr i -1} {
	if {[info exists SM($i)]} {
	    set SMList [lsort -index 1 -decreasing $SM($i)]
	    set msg "\n  SM - [lindex $tmpStateList $i]"
	    if {$_fileName == "stdout"} {
		inform "-I-ibdiagnet:SM.report.head" [lindex $tmpStateList $i]
	    } else {
		puts $_fileName $msg
	    }
	    foreach element $SMList {
		set tmpDirectPath [lindex $element 0]
		set nodeName [DrPath2Name $tmpDirectPath -port [GetEntryPort $tmpDirectPath] -fullName]
		if { $tmpDirectPath == "" } {
		    set nodeName "$G(var:desc.local.dev) : $nodeName"
		}
		set msg "    $nodeName priority:[lindex $element 1]"
		if {$_fileName == "stdout"} {
		    inform "-I-ibdiagnet:SM.report.body" $nodeName [lindex $element 1]
		} else {
		    puts $_fileName $msg
		}
	    }
	}
    }
    return 0
}
##############################

######################################################################
### Partitions Checking and Reporting
######################################################################
# return the list of all pkeys from the given port
proc GetPortPkeys {drPath portNum numPKeys} {
    set numBlocks [expr ($numPKeys + 31) / 32]
    set pkeys {}
    # get the pkey table of the port
    for {set block 0} {$block < $numBlocks} {incr block} {
	if {[catch {
	    set pkeyTable [SmMadGetByDr PkeyTable dump "$drPath" $portNum $block]
	} e]} {
	    inform "-E-ibdiagnet:PKeys.getPkey" $drPath $portNum $block
	    continue
	}
	foreach pkey $pkeyTable {
	    if {$pkey != 0} {
		lappend pkeys $pkey
	    }
	}
    }
    return $pkeys
}

# check the rules of partition enforcement on the fabric and report
# Define the G(data:CAPortPKeys:<nodeGuid>:<PN>)
# and G(data:PKeyNodePorts:<pkey-base>) {{<nodeGuid> <PN>} ....}
proc CheckPartitions {} {
    global G Neighbor
    inform "-I-ibdiagnet:PKeys.report.header"

    if {[CheckSkipStatus part]} {
        inform "-I-reporting:skip.set.no.report"
        return 1
    }
	
    # go over all HCA ports and get their PKey tables
    foreach nodeGuidPortNum [array names G data:PortGuid.*:*] {
	regexp {PortGuid.([^:]+):([^:]+)} $nodeGuidPortNum d1 nodeGuid portNum
	set portGuid $G($nodeGuidPortNum)
	
	# we need to examine the NodeInfo to see CA and how many PKey blocks
	if {[catch {set nodeInfo $G(data:NodeInfo.$nodeGuid)}]} {
	    inform "-W-ibdiagnet:PKeys.noNodeInfo" $nodeGuid
	    continue
	}

	# ignore switches
	set nodeType [GetWordAfterFlag $nodeInfo -node_type]
	if {$nodeType == 2} {continue}

	set drPath $G(data:dr.path.to.guid.$portGuid)

	set numPKeys [GetWordAfterFlag $nodeInfo -partition_cap]
	set pkeys [GetPortPkeys $drPath $portNum $numPKeys]
	set G(data:CAPortPKeys:$nodeGuid:$portNum) $pkeys
	
	# we might have direct enforcement by the neighbor switch
	set neighNodeNPort $Neighbor($nodeGuid:$portNum)
	set remPKeys {}

	foreach {remNodeGuid remPortNum} [split $neighNodeNPort :] {break}
	if {[catch {set remNodeInfo $G(data:NodeInfo.$remNodeGuid)}]} {
	    inform "-W-ibdiagnet:PKeys.noNodeInfo" $remNodeGuid
	} else {
	    if {[GetWordAfterFlag $remNodeInfo -node_type] == 2} {
		# is the remote PortInfo enforcing pkeys:
		set portInfo $G(data:PortInfo.$remNodeGuid:$remPortNum)
		set opvl_enforce [GetWordAfterFlag $portInfo -vl_enforce]
		set outEnforce [expr $opvl_enforce & 0x4]
		set inEnforce  [expr $opvl_enforce & 0x8]
		
		set remDrPath $G(data:dr.path.to.node.$remNodeGuid)
		if {$outEnforce || $inEnforce} {
		    if {! ($outEnforce && $inEnforce)} {
			set swNodeName [DrPath2Name $remDrPath -port [GetEntryPort $remDrPath] -fullName]
			inform "-W-ibdiagnet:PKeys.in.out.not.same" $swNodeName $inEnforce $outEnforce
		    }
		    # the switch info to see how many pkeys:
		    if {[catch {set partcap [SmMadGetByDr SwitchInfo -enforce_cap "$remDrPath"]}]} {
			inform "-W-ibdiagnet:PKeys.noSwitchInfo" $remNodeGuid $remDrPath
			set remPKeys {}
		    } else {
			set remPKeys [GetPortPkeys $remDrPath $remPortNum $partcap]
			if {[llength $remPKeys] == 0} {
			    set remPKeys 0x0
			}
		    }
		}
	    }
	}
	
	# filter pkeys by remote port if exist
	if {[info exists REM_PKEY]} {unset REM_PKEY}
	if {[llength $remPKeys]} {
	    foreach pkey $remPKeys {
		set base [expr $pkey & 0x7fff]
		set REM_PKEY($base) [expr $pkey & 0x8000]
	    }
	}
	
	foreach pkey $pkeys {
	    set base [expr $pkey & 0x7fff]
	    
	    if {$pkey != 0} {
		if {$base != $pkey} {
		    set isPartial 0
		} else {
		    set isPartial 1
		}
		
		# see that remote pkeys do not filter
		if {[llength $remPKeys]} {
		    if {![info exists REM_PKEY($base)]} {
			set nodeName [DrPath2Name $drPath -port [GetEntryPort $drPath] -fullName]
			inform "-W-ibdiagnet:PKeys.switch.missing.pkey" $nodeName $pkey
			continue
		    } else {
			if {!$REM_PKEY($base) && !$isPartial} {
			    set nodeName [DrPath2Name $drPath -port [GetEntryPort $drPath] -fullName]
			    inform "-W-ibdiagnet:PKeys.switch.part.pkey" $nodeName $pkey
			    set isPartial 1
			}
		    }
		}
		if {![info exists PKEY_HOSTS($base)]} {
		    set PKEY_HOSTS($base) "{$portGuid $isPartial}"
		} else {
		    lappend PKEY_HOSTS($base) "$portGuid $isPartial"
		}
		if {![info exists G(data:PKeyNodePorts:$base)]} {
		    set G(data:PKeyNodePorts:$base) [list [list $nodeGuid $portNum]]
		} else {
		    lappend G(data:PKeyNodePorts:$base) [list $nodeGuid $portNum]
		}
	    }
	}
	# If remote port is SW and PKey enforcement is ON on that port get it
    }

    # report partitions:
    set FileID [InitializeOutputFile $G(var:tool.name).pkey]
    foreach pkey [lsort -integer [array names PKEY_HOSTS]] {
	set full 0
	set part 0
	set num [llength $PKEY_HOSTS($pkey)]
	puts $FileID "GROUP PKey:[format 0x%04x $pkey] Hosts:$num"
	# report each host port of the group
	foreach portGuidNPartial $PKEY_HOSTS($pkey) {
	    set portGuid [lindex $portGuidNPartial 0]
	    set isPartial [lindex $portGuidNPartial 1]
	    
	    set drPath $G(data:dr.path.to.guid.$portGuid)
	    set nodeName [DrPath2Name $drPath -port [GetEntryPort $drPath] -fullName]
	    if {$isPartial} {
		incr part
		puts $FileID "   Part $nodeName"
	    } else {
		incr full
		puts $FileID "   Full $nodeName"
	    }
	}
	inform "-I-ibdiagnet:PKeys.Group" $pkey $num $full $part
	puts $FileID [Bar - 80]
    }
    close $FileID
    return 0
}

# return the list of all pkeys from the given port
# return "Not-Enforced" if no enforcement on switch port
proc GetPortPkeysByDRPortNumAndDirection {drPath portNum dir} {

    # we need to examine the NodeInfo to see CA and how many PKey blocks
    if {[catch {set nodeInfo [SmMadGetByDr NodeInfo dump "$drPath"]}]} {
	puts "-E- failed to get src port guid for path:$drPath"
	return ""
    }

    set nodeType [GetWordAfterFlag $nodeInfo -node_type]
    if {$nodeType == 2} {

	# check it has partition enforcement on this port
	set nodeGuid [GetWordAfterFlag $nodeInfo -node_guid]
	# is a switch - use switch info ...
	if {[catch {set portInfo [SmMadGetByDr PortInfo dump "$drPath" $portNum]}]} {
	    inform "-W-ibdiagnet:PKeys.noPortInfo" $drPath $portNum
	    return ""
	}
	set opvl_enforce [GetWordAfterFlag $portInfo -vl_enforce]
	set outEnforce [expr $opvl_enforce & 0x4]
	set inEnforce  [expr $opvl_enforce & 0x8]
	
	if {$outEnforce || $inEnforce} {
	    if {! ($outEnforce && $inEnforce)} {
		set nodeName [DrPath2Name $drPath -port [GetEntryPort $drPath] -fullName]
		inform "-W-ibdiagnet:PKeys.in.out.not.same" $nodeName $inEnforce $outEnforce
	    }
	    if {($dir == "in" && !$inEnforce) || ($dir == "out" && !$outEnforce)} {
		return "Not-Enforced"
	    }
	} else {
	    return "Not-Enforced"
	}
	
	# the switch info to see how many pkeys:
	if {[catch {set numPKeys [SmMadGetByDr SwitchInfo -enforce_cap "$drPath"]}]} {
	    inform "-W-ibdiagnet:PKeys.noSwitchInfo" $nodeGuid $drPath
	    return ""
	}
    } else {
	set numPKeys [GetWordAfterFlag $nodeInfo -partition_cap]
    }

    return [GetPortPkeys $drPath $portNum $numPKeys]
}

# Perform partition analysis of the path
# store the final path partitions in G(data:path.partitions)
proc AnalyzePathPartitions {paths} {
    global G Neighbor

    inform "-I-ibdiagpath:PKeys.report.header"

    # find the source port
    if {[llength $paths] > 1} {
	set srcPath [lindex $paths 0]
    } else {
	set srcPath ""
    }
    set dstPath [lindex $paths end]

    if {[catch {set NodeInfo [SmMadGetByDr NodeInfo dump "$srcPath"]}]} {
	"-E-ibdiagpath:PKeys.FailNodeInfo" $srcPath
	return 1
    }
    set srcNodeGuid [GetWordAfterFlag $NodeInfo "-node_guid"]
    set srcPortGuid [GetWordAfterFlag $NodeInfo "-port_guid"]
    set srcPortNum  [GetEntryPort $srcPath -byNodeInfo $NodeInfo]

    set pkeys [GetPortPkeysByDRPortNumAndDirection $srcPath $srcPortNum "out"]
    foreach pkey $pkeys {
	if {$pkey == "Not-Enforced"} {continue}
	set base [expr $pkey & 0x7fff]
	set SRC_PKEYS($base) $pkey
    }
    set nodeName [DrPath2Name $srcPath -port [GetEntryPort $srcPath] -fullName]
    inform "-I-ibdiagpath:PKeys.src.pkeys" $nodeName $srcPortNum $pkeys

    # get the remote port of the SRC
    set neighNodeNPort $Neighbor($srcNodeGuid:$srcPortNum)
    foreach {remNodeGuid remPortNum} [split $neighNodeNPort :] {break}

    # Now go over the rest of the path:
    if {[llength $srcPath]} {
	set startIdx [expr [llength $srcPath] - 2]
    } else {
	set startIdx 0
    }
    for {set idx $startIdx} {$idx < [llength $dstPath]} {incr idx} {
	set drPath [lrange $dstPath 0 $idx]
	if {$idx + 1 < [llength $dstPath]} {
	} else {
	    set nextPortNum 0
	}

	set stagePaths "in $remPortNum "
	if {$idx + 1 < [llength $dstPath]} {
	    set    nextPortNum [lindex  $dstPath [expr $idx + 1]]
	    append stagePaths "out $nextPortNum"
	}

	# loop on in/out
	foreach {dir portNum} $stagePaths {
	    if {$portNum == 0} {continue}

	    if {[catch {set NodeInfo [SmMadGetByDr NodeInfo dump "$drPath"]}]} {
		inform "-E-ibdiagpath:PKeys.FailNodeInfo" $drPath
		return 1
	    }
	    set nodeGuid [GetWordAfterFlag $NodeInfo "-node_guid"]
	    set portGuid [GetWordAfterFlag $NodeInfo "-port_guid"]
	    set pkeys [GetPortPkeysByDRPortNumAndDirection $drPath $portNum $dir]
	    set nodeName [DrPath2Name $drPath -fullName]
	    inform "-V-ibdiagpath:PKeys.portPkeys" $nodeName $portNum $dir $pkeys
	    # must get some pkeys and make sure not to filter if not enforced
	    if {[llength $pkeys] && ([lindex $pkeys 0] != "Not-Enforced")} {
		if {[info exist PKEYS]} {unset PKEYS}
		foreach pkey $pkeys {
		    set base [expr $pkey & 0x7fff]
		    set PKEYS($base) $pkey
		}
		foreach base [array names SRC_PKEYS] {
		    if {![info exists PKEYS($base)]} {
			inform "-W-ibdiagpath:PKeys.blockOnPath" $nodeName $dir $SRC_PKEYS($base)
			unset SRC_PKEYS($base)
		    }
		}
	    }
	}

	if {$dir == "out"} {
	    # get the remote node info by DR and extract input port
	    lappend drPath $portNum
	    if {[catch {set remPortNum [SmMadGetByDr PortInfo -local_port_num "$drPath" 1]}]} {
		inform "-E-ibdiagpath:PKeys.FailPortInfo" $drPath
		return 1
	    }
	}
    }
    inform "-I-ibdiagpath:PKeys.dst.pkeys" $nodeName $pkeys

    # make sure we have some shared PKeys left:
    set shared {}
    foreach base [array names SRC_PKEYS] {
	if {[info exists PKEYS($base)]} {
	    set isSrcFull [expr 0x8000 & $SRC_PKEYS($base)]
	    set isPathFull [expr 0x8000 & $PKEYS($base)]
	    if { $isSrcFull || $isPathFull} {
		lappend shared $SRC_PKEYS($base)
	    }
	}
    }

    set G(data:path.partitions) $shared
    if {[llength $shared]} {
	inform "-I-ibdiagpath:PKeys.path.shared" $shared
    } else {
	inform "-E-ibdiagpath:PKeys.path.noShared"
    }
    return 0
}
##############################

######################################################################
### IPoIB Checking and Reporting
######################################################################

# obtain the rate (code and Gbps) of the given portInfo
proc GetPortInfoRateCodeAndGbps {portInfo} {
    set width [GetWordAfterFlag $portInfo -link_width_active]
    set speed [expr ([GetWordAfterFlag $portInfo -link_speed] & 0xf0) >> 4]

    if {[lsearch -exact {1 2 4 8} $width] < 0} {
	puts "Error: unrecognized link width code:$width"
	return ""
    }

    # coding
    # speed 1=2.5G 2=5G 4=10G
    # width 1=1X 2=4X 4=8X 8=12X
    switch $speed {
	1 {
	    switch $width {
		1 {set rate 2; set gbps 2.5}
		2 {set rate 3; set gbps 10 }
		4 {set rate 6; set gbps 20 }
		8 {set rate 4; set gbps 30 }
	    }
	}
	2 {
	    switch $width {
		1 {set rate 5; set gbps 5}
		2 {set rate 6; set gbps 20 }
		4 {set rate 7; set gbps 40 }
		8 {set rate 8; set gbps 60 }
	    }
	}
	4 {
	    switch $width {
		1 {set rate 3; set gbps 10 }
		2 {set rate 7; set gbps 40 }
		4 {set rate 9; set gbps 80 }
		8 {set rate 10; set gbps 120 }
	    }
	}
	default {
	    puts "Error: unrecognized link speed code:$speed"
	    return ""
	}
    }
    return [list $rate $gbps]
}

proc GetRateGbps {rate} {
    switch $rate {
	2 {set gbps 2.5}
	3 {set gbps 10}
	4 {set gbps 30}
	5 {set gbps 5}
	6 {set gbps 20}
	7 {set gbps 40}
	8 {set gbps 60}
	9 {set gbps 80}
	10 {set gbps 120}
	default {set gbps 0}
    }
    return $gbps
}

proc GetGbpsRate {gbps} {
    switch $gbps {
	2.5 {set rate 2}
	5   {set rate 5}
	10  {set rate 3}
	20  {set rate 6}
	30  {set rate 4}
	40  {set rate 7}
	60  {set rate 8}
	80  {set rate 9}
	120 {set rate 10}
	default {set rate 0}
    }
    return $rate
}

# perform a subnet global check of IPoIB subnets
proc CheckIPoIB {} {
    global G

    inform "-I-ibdiagnet:ipoib.header"

    if {[CheckSkipStatus ipoib]} {
        inform "-I-reporting:skip.set.no.report"
        return 1
    }
	
    # obtain the list of IPoIB MCGs from the SA
    set pKeyRex {[0-9a-fA-F]:4}
    foreach mcg [sacMCMQuery getTable 0] {
	set mgid [sacMCMRec_mgid_get $mcg]
	# check to see if IPoIB mcg
	if {[regexp {0xff12401b([0-9a-fA-F]{4})0000:0x00000000ffffffff} $mgid d1 p]} {
	    set pkey [expr 0x$p & 0x7fff]
	    set IPoIB_MCGS($mcg) "$pkey 4"
	    set IPV4_MCGS($pkey) $mcg
	} elseif {[regexp {0xff12601b([0-9a-fA-F]{4})0000:0x00000000ffffffff} $mgid d1 p]} {
	    set pkey [expr 0x$p & 0x7fff]
	    set IPoIB_MCGS($mcg) "$pkey 6"
	    set IPV6_MCGS($pkey) $mcg
	}
    }

    # no go over all subnets and check their rules:
    foreach mcg [array names IPoIB_MCGS] {
	set gMtu  [expr [sacMCMRec_mtu_get $mcg]& 0x3f]
	set gRate [expr [sacMCMRec_rate_get $mcg]& 0x3f]
	set gSL   [expr ([sacMCMRec_sl_flow_hop_get $mcg] & 0xf000) >> 24]
	set gPKey [sacMCMRec_pkey_get $mcg]
	set gQKey [sacMCMRec_qkey_get $mcg]
	foreach {pkey IPVersion} $IPoIB_MCGS($mcg) {break}

	inform "-I-ipoib.subnet" $IPVersion $pkey $gMtu $gRate $gSL $gPKey $gQKey
	
	if {[expr 0x7fff & $gPKey] != $pkey} {
	    inform "-W-ipoib.bad.pkey" $gPKey $pkey
	}

	# go over all the members of the partition and see if they can join
	# collecting their minimal rate
	if {[catch {set pkeyMembers $G(data:PKeyNodePorts:$pkey)}]} {
	    inform "-W-ibdiagnet.ipoib.noMemers"
	    continue
	}

	# obtain the rate of the port and track min rate
	set gGbps [GetRateGbps $gRate]
	set minGbps 120
	foreach nodeNPort $pkeyMembers {
	    foreach {nodeGuid portNum} $nodeNPort {break}
	    set portInfo $G(data:PortInfo.$nodeGuid:$portNum)
	    
	    set rateNGbps [GetPortInfoRateCodeAndGbps $portInfo]
	    if {[llength $rateNGbps] != 2} {
		set drPath $G(data:dr.path.to.node.$nodeGuid)
		set name [DrPath2Name $drPath -port [GetEntryPort $drPath] -fullName]
		inform "-E-ipoib.ilegalRate" $name
	    }
	    foreach {rate gbps} $rateNGbps {break}
	    if {$gbps < $gGbps} {
		set drPath $G(data:dr.path.to.node.$nodeGuid)
		set name [DrPath2Name $drPath -port [GetEntryPort $drPath] -fullName]
		inform "-W-ipoib.cantJoin" $name $rate $gRate
	    } else {
		if {$minGbps > $gbps} {
		    set minGbps $gbps
		}
	    }
	}
	if {$minGbps > $gGbps} {
	    set minRate [GetGbpsRate $minGbps]
	    inform "-W-ibdiagnet.ipoib.rateToLow" $minRate $gRate
	}
    }
}

# perform a path based check of IPoIB subnets
proc CheckPathIPoIB {paths} {
    global G

    inform "-I-ibdiagpath:ipoib.header"

    # find the source port
    if {[llength $paths] > 1} {
	set srcPath [lindex $paths 0]
    } else {
	set srcPath ""
    }
    if {[catch {set srcNodeInfo [SmMadGetByDr NodeInfo dump "$srcPath"]}]} {
	"-E-ibdiagpath:PKeys.FailNodeInfo" $srcPath
	return 1
    }
    set srcNodeGuid [GetWordAfterFlag $srcNodeInfo "-node_guid"]
    set srcPortGuid [GetWordAfterFlag $srcNodeInfo "-port_guid"]
    set srcPortNum  [GetEntryPort $srcPath -byNodeInfo $srcNodeInfo]

    set dstPath [lindex $paths end]
    if {[catch {set dstNodeInfo [SmMadGetByDr NodeInfo dump "$dstPath"]}]} {
	"-E-ibdiagpath:PKeys.FailNodeInfo" $dstPath
	return 1
    }
    set dstNodeGuid [GetWordAfterFlag $dstNodeInfo "-node_guid"]
    set dstPortGuid [GetWordAfterFlag $dstNodeInfo "-port_guid"]
    set dstPortNum  [GetEntryPort $dstPath -byNodeInfo $dstNodeInfo]

    set pkeyMembers [list \
			 [list $srcNodeGuid $srcPath $srcPortNum] \
			 [list $dstNodeGuid $dstPath $dstPortNum]]

    # provied by AnalyzePathPartitions
    set pathBasePkeys {}
    foreach pkey $G(data:path.partitions) {
	lappend pathBasePkeys [expr $pkey & 0x7fff]
    }

    # obtain the list of IPoIB MCGs from the SA
    set pKeyRex {[0-9a-fA-F]:4}
    foreach mcg [sacMCMQuery getTable 0] {
	set mgid [sacMCMRec_mgid_get $mcg]
	# check to see if IPoIB mcg
	if {[regexp {0xff12401b([0-9a-fA-F]{4})0000:0x00000000ffffffff} $mgid d1 p]} {
	    set pkey [expr 0x$p & 0x7fff]
	    set IPoIB_MCGS($mcg) "$pkey 4"
	    set IPV4_MCGS($pkey) $mcg
	} elseif {[regexp {0xff12601b([0-9a-fA-F]{4})0000:0x00000000ffffffff} $mgid d1 p]} {
	    set pkey [expr 0x$p & 0x7fff]
	    set IPoIB_MCGS($mcg) "$pkey 6"
	    set IPV6_MCGS($pkey) $mcg
	}
    }

    # no go over all subnets and check their rules:
    set anyGroup 0
    foreach mcg [array names IPoIB_MCGS] {
	foreach {pkey IPVersion} $IPoIB_MCGS($mcg) {break}

	if {[lsearch -exact $pathBasePkeys $pkey] < 0} {continue}

	set gMtu  [expr [sacMCMRec_mtu_get $mcg]& 0x3f]
	set gRate [expr [sacMCMRec_rate_get $mcg]& 0x3f]
	set gSL   [expr ([sacMCMRec_sl_flow_hop_get $mcg] & 0xf000) >> 24]
	set gPKey [sacMCMRec_pkey_get $mcg]
	set gQKey [sacMCMRec_qkey_get $mcg]

	inform "-I-ipoib.subnet" $IPVersion $pkey $gMtu $gRate $gSL $gPKey $gQKey

	if {[expr 0x7fff & $gPKey] != $pkey} {
	    inform "-W-ipoib.bad.pkey" $gPKey $pkey
	}

	# go over all the members of the partition and see if they can join

	# obtain the rate of the port and track min rate
	set gGbps [GetRateGbps $gRate]
	set anyFail 0
	foreach nodeGuidPortGuidNPort $pkeyMembers {
	    foreach {nodeGuid drPath portNum} $nodeGuidPortGuidNPort {break}
	    if {[catch {set portInfo [SmMadGetByDr PortInfo dump "$drPath" 1]}]} {
		inform "-E-ibdiagpath:PKeys.FailPortInfo" $drPath
		return 1
	    }

	    set rateNGbps [GetPortInfoRateCodeAndGbps $portInfo]
	    if {[llength $rateNGbps] != 2} {
		set name [DrPath2Name $drPath -port [GetEntryPort $drPath] -fullName]
		inform "-E-ipoib.ilegalRate" $name
	    }
	    foreach {rate gbps} $rateNGbps {break}
	    if {$gbps < $gGbps} {
		set name [DrPath2Name $drPath -port [GetEntryPort $drPath] -fullName]
		inform "-W-ipoib.cantJoin" $name $rate $gRate
		incr anyFail
	    }
	}
	if {$anyFail == 0} {
	    incr anyGroup
	}
    }
    if {$anyGroup == 0} {
	inform "-E-ibdiagpath.ipoib.noGroups"
    }
    return 0
}
######################################################################

######################################################################
### QoS Checking and Reporting
######################################################################

# perform the QoS check over a path
proc CheckPathQoS {paths} {
    global G Neighbor

    inform "-I-ibdiagpath:qos.report.header"

    # we track SLs that are OK in this array:
    for {set sl 0} {$sl < 16} {incr sl} {
	set BLOCKED_SL($sl) 0
    }

    # find the source port
    if {[llength $paths] > 1} {
	set srcPath [lindex $paths 0]
    } else {
	set srcPath ""
    }
    set dstPath [lindex $paths end]
    if {[catch {set NodeInfo [SmMadGetByDr NodeInfo dump "$srcPath"]}]} {
	"-E-ibdiagpath:Qos.FailNodeInfo" $srcPath
	return 1
    }
    set nodeGuid [GetWordAfterFlag $NodeInfo "-node_guid"]
    set portGuid [GetWordAfterFlag $NodeInfo "-port_guid"]
    set outPortNum [GetEntryPort $srcPath -byNodeInfo $NodeInfo]
    # don't care for CA and assume path from a switch starts at port 0
    set inPortNum 0
    set drPath $srcPath

    # Now go over the rest of the path:
    if {[llength $srcPath]} {
	set idx [expr [llength $srcPath] - 2]
    } else {
	set idx 0
    }

    set done 0

    while {!$done} {
	# report stage
	set name [DrPath2Name $drPath -fullName]
	inform "-V-ibdiagpath.qos.atNode" $name $inPortNum $outPortNum

	# obtain OPVLs and VLA Cap from PortInfo for the outPort
	if {[catch {set portInfo [SmMadGetByDr PortInfo dump "$drPath" $outPortNum]}]} {
	    inform "-E-ibdiagpath:qos.FailPortInfoOpVLs" $drPath
	    return 1
	}
	set vlEnforce [GetWordAfterFlag $portInfo "-vl_enforce"]
	set opVLs [expr ($vlEnforce & 0xf0) >> 4 - 1]
	set vlaHighCap [GetWordAfterFlag $portInfo "-vl_arb_high_cap"]
	set vlaLowCap  [GetWordAfterFlag $portInfo "-vl_arb_high_cap"]

	# get the SL2VL (inPort,outPort)
	if {[catch {set SL2VL [SmMadGetByDr SlVlTable dump "$drPath" $inPortNum $outPortNum]}]} {
	    inform "-E-ibdiagpath:qos.FailSL2VL" $drPath
	    return 1
	}
	# get VLA(outPort) both High and Low
	set VLArbLow {}
	for {set i 0} {$i < ($vlaLowCap + 31)/32} {incr i} {
	    if {[catch {set vlaBlock [SmMadGetByDr VlArbTable dump "$drPath" $outPortNum [expr $i + 1]]} e]} {
		inform "-E-ibdiagpath:qos.FailVLArb" $drPath
		return 1
	    }
	    foreach entry $vlaBlock {
		lappend VLArbLow $entry
	    }
	}
	set VLArbHigh {}
	for {set i 0} {$i < ($vlaLowCap + 31)/32} {incr i} {
	    if {[catch {set vlaBlock [SmMadGetByDr VlArbTable dump "$drPath" $outPortNum [expr $i + 3]]}]} {
		inform "-E-ibdiagpath:qos.FailVLArb" $drPath
		return 1
	    }
	    foreach entry $vlaBlock {
		lappend VLArbHigh $entry
	    }
	}
	# report VLA VLs > OPVLs
	# report and track blocked VLs in VLA
	set lowHighCtrl [list Low $vlaLowCap $VLArbLow High $vlaHighCap $VLArbHigh]
	if {[info exist VL_WEIGHT]} {unset VL_WEIGHT}
	foreach {tbl numEntries values} $lowHighCtrl {
	    set overRangeVLs {}
	    for {set i 0} {$i < $numEntries} {incr i} {
		set entry [lindex $values $i]
		set vl [expr [lindex $entry 0]]
		set weight [lindex $entry 1]
		if {$vl >= $opVLs} {
		    lappend overRangeVLs $i
		} else {
		    if {$weight > 0} {
			set VL_WEIGHT($vl) $weight
		    }
		}
	    }
	    if {[llength $overRangeVLs]} {
		inform "-W-ibdiagpath:qos.vlaOverOpVLs" $name $outPortNum $overRangeVLs $opVLs $tbl
	    }
	}

	set blockedVLs {}
	for {set vl 0} {$vl < $opVLs} {incr vl} {
	    if {![info exists VL_WEIGHT($vl)]} {
		lappend blockedVLs $vl
	    }
	}
	if {[llength $blockedVLs]} {
	    inform "-W-ibdiagpath:qos.blockedVLs" $name $outPortNum $blockedVLs
	}

	# report SLs that map to VL > OPVL
	# report and track SLs that blocked by blocked VLs
	set outOfRangeVLsSLs {}
	set blockedSLs {}
	for {set i 0} {$i < 8} {incr i} {
	    set sl0 [expr 2*$i]
	    set sl1 [expr 2*$i + 1]
	    set vl0 [expr ([lindex $SL2VL $i] & 0xf0) >> 4]
	    set vl1 [expr [lindex $SL2VL $i] & 0xf]
	    if {($vl0 >= $opVLs) || ($vl0 == 15)} {
		set BLOCKED_SL($sl0) 1
		lappend outOfRangeVLsSLs $sl0
	    }
	    if {($vl1 >= $opVLs) || ($vl1 == 15)} {
		set BLOCKED_SL($sl1) 1
		lappend outOfRangeVLsSLs $sl1
	    }
	    
	    if {[lsearch -exact $blockedVLs $vl0] >= 0} {
		set BLOCKED_SL($sl0) 1
		lappend blockedSLs $sl0
	    }
	    if {[lsearch -exact $blockedVLs $vl1] >= 0} {
		set BLOCKED_SL($sl1) 1
		lappend blockedSLs $sl1
	    }
	}
	if {[llength $outOfRangeVLsSLs]} {
	    inform "-W-ibdiagpath:qos.sl2vlOORange" $name $inPortNum $outPortNum $opVLs \
		[lsort -integer $outOfRangeVLsSLs]
	}
	if {[llength $blockedSLs]} {
	    inform "-W-ibdiagpath:qos.blockedSL" $name $inPortNum $outPortNum \
		[lsort -integer $blockedSLs]
	}

	# obtain next path
	set drPath [lrange $dstPath 0 $idx]
	
	if {$idx + 1 == [llength $dstPath]} {
	    set done 1
	    continue
	}

	# get the next nodeGuid
	if {[catch {set NodeInfo [SmMadGetByDr NodeInfo dump "$drPath"]}]} {
	    "-E-ibdiagpath:Qos.FailNodeInfo" $drPath
	    return 1
	}
	set nodeGuid [GetWordAfterFlag $NodeInfo "-node_guid"]
	set portGuid [GetWordAfterFlag $NodeInfo "-port_guid"]
	set inPortNum [GetEntryPort $drPath -byNodeInfo $NodeInfo]
	set outPortNum [lindex $dstPath [expr $idx + 1]]
	incr idx
    }

    set pathSLs {}
    foreach sl [lsort -integer [array names BLOCKED_SL]] {
	if {$BLOCKED_SL($sl) == 0} {
	    lappend pathSLs $sl
	}
    }
    if {[llength $pathSLs]} {
	inform "-I-ibdiagpath:qos.pathSLs" $pathSLs
    } else {
	inform "-E-ibdiagpath:qos.noPathSLs"
    }
    return 0
}
######################################################################

######################################################################
### If a topology file is given
######################################################################

proc MatchTopology { lstFile args } {
    global G

    if {[info exists G(lst.failed)]} {
	inform "-F-crash:failed.build.lst"
	return 1
    }
    if {[CheckSkipStatus load_ibdm]} {
	return 1
    }
    if { [info exists G(argv:report)] || [info exists G(argv:topo.file)] } {
	set G(IBfabric:.lst) [new_IBFabric]
	if {[IBFabric_parseSubnetLinks $G(IBfabric:.lst) $lstFile]} {
	    inform "-F-crash:failed.parse.lst"
	}
    }
    if { ! [info exists G(argv:topo.file)] } {
	return 1
    }

    # Matching defined and discovered fabric
    if { [info exists G(LocalDeviceDuplicated)] } {
	if {[info exists G(argv:topo.file)] && $G(bool:sys.name.guessed)} {
	    inform "-E-topology:localDevice.Duplicated"
	    return 1
	}
    }
    set MatchingResult \
	[ibdmMatchFabrics $G(IBfabric:.topo) $G(IBfabric:.lst) \
	     $G(argv:sys.name) $G(argv:port.num) $G(data:root.port.guid) ]

    switch -- [lrange $MatchingResult 0 4] {
	"Fail to find anchor port" -
	"Even starting ports do not" {
	    inform "-W-topology:Critical.mismatch" -massage [join $MatchingResult]
	    return 1
	}
    }

    set G(MatchingResult) ""
    set old_line ""
    set G(missing.links) ""
    foreach line [split $MatchingResult \n] {
	if { [regexp {[^ ]} $line] || [regexp {[^ ]} $old_line] } {
	    lappend G(MatchingResult) "  $line"
	}
	# $G(missing.links) is the list of links found to be missing by topology
	# matching;
	# a pair of entries (0 & 1 , 2 & 3 etc.) are ports at the link's end
	set missingSysExp \
	    {^ *Missing System:([^ \(]+).*from port: *([^ ]+) to: *([^ ]+) *$}
	set missingLinkExp \
	    {^ *Missing internal Link connecting: *([^ ]+) to: *([^ ]+) *$}
	if { [regsub $missingSysExp "$old_line $line" {\1/\2 \3} link] || \
		 [regsub $missingLinkExp  "$line" {\1 \2} link] } {
	    set G(missing.links) [concat $G(missing.links) $link]
	}
	set old_line $line
    }

    set G(IBfabric:merged) [new_IBFabric]
    if [catch {ibdmBuildMergedFabric \
		   $G(IBfabric:.topo) $G(IBfabric:.lst) $G(IBfabric:merged)} ] {
	return 1
    }

    # need to copy the min lid
    IBFabric_minLid_set $G(IBfabric:merged) [IBFabric_minLid_get $G(IBfabric:.lst)]

    return 0
}
##############################

##############################
proc DumpTopologyMatching { args } {
    global G
    if {$G(bool:topology.matched) == 0} { return }
    set noheader [BoolWordInList "-noheader" $args]
    if { ! $noheader } { inform "-I-topology:matching.header" }

    set MatchingResultLen [llength $G(MatchingResult)]
    if { $MatchingResultLen == 0 } {
	inform "-I-topology:matching.perfect"
    } else {
	if { ! $noheader } { inform "-I-topology:matching.note" }
	if { $MatchingResultLen > $G(var:warn.long.matching.results) } {
	    inform "-W-topology:matching.bad"
	}
    }
    if {[string is space [lindex $G(MatchingResult) end]]} {
	set G(MatchingResult) [lrange $G(MatchingResult) 0 end-1]
    }
    PutsIn80Chars [join $G(MatchingResult) \n]
    return 0
}
##############################

proc ArrangeDR {_dr} {
    set res ""
    foreach drEntry $_dr {
	append res $drEntry,
    }
    return [string range $res 0 end-1]
}

##############################
# support LID , PortGUID , NodeGUID , EntryPort , Type , DevID ,Name
proc DrPath2Name { DirectPath args } {
    global G
    set fullName [BoolWordInList "-fullName" $args]
    set nameOnly [BoolWordInList "-nameOnly" $args]
    set nameLast [BoolWordInList "-nameLast" $args]
    if {[BoolWordInList "-byDr" $args]} {
	set byDr "-byDr"
    } else {
	set byDr ""
    }
    if {[set addPort [BoolWordInList "-port" $args]]} {
	set port [GetWordAfterFlag $args "-port"]
	set EntryPort $port
	if {$EntryPort == ""} {
	    set EntryPort 0
	}
    } elseif {[catch {set EntryPort [GetEntryPort $DirectPath]}]} {
	set EntryPort 0
    } else {
	if {$EntryPort == ""} {
	    set EntryPort 0
	}
    }
    if { $fullName && [PathIsBad $DirectPath] < 2} {
	set PortGUID   [GetParamValue PortGUID $DirectPath]
	set NodeDevID  [expr [GetParamValue DevID $DirectPath]]
	set NodePorts  [GetParamValue Ports $DirectPath]
	set NodeLid [GetParamValue LID $DirectPath -port $EntryPort]
	set lidGuidDev "lid=$NodeLid guid=$PortGUID dev=$NodeDevID"
    } else {
	set lidGuidDev ""
    }
    if { ($G(bool:topology.matched)==0) } {
	if {![catch {set deviceType [GetParamValue Type $DirectPath $byDr]}]} {
	    if {$deviceType == "CA"} {
		if {![catch {set nodeDesc [GetParamValue NodeDesc $DirectPath $byDr]}]} {
		    if {($nodeDesc == "") && ($addPort)} {
			set res "Port=$port"
		    } else {
			set res ""
			set hca_idx ""
			scan $nodeDesc {%s %s} res hca_idx
			if {$hca_idx != ""} {
			    set hca_idx [string range $hca_idx 4 end]
			    if {([string is integer $hca_idx]) && ($hca_idx != 1)} {
				append res "/U$hca_idx"
			    }
			}
			if {($addPort)} {
			    append res "/P$port"
			}
		    }
		    if {([llength $lidGuidDev] != 0) && !$nameOnly} {
			if {$nameLast} {
			    set res "$lidGuidDev $res"
			} else {
			    set res "$res $lidGuidDev"
			}
		    }
		    return $res
		}
	    }
	}
	if {($addPort)} {
	    set res "Port=$port"
	} else {
	    set res ""
	}
	if {([llength $lidGuidDev] != 0) && !$nameOnly} {
	    if {$nameLast} {
		set res "$lidGuidDev $res"
	    } else {
		set res "$res $lidGuidDev"
	    }
	}
	return $res
    }
    set path $DirectPath
    set topoNodesList [join [IBFabric_NodeByName_get $G(IBfabric:.topo)]]
    if { [set nodePointer [GetWordAfterFlag $topoNodesList $G(argv:sys.name)]] == "" } {
	if {($addPort)} {
	    return "$lidGuidDev port=$port"
	} else {
	    return "$lidGuidDev"
	}
    }
    while { [llength $path] > 0 } {
	set port [lindex $path 0]
	set path [lrange $path 1 end]

	set nodePorts  [IBNode_Ports_get $nodePointer]
	set portPointer [IBNode_getPort $nodePointer $port]

	if {$portPointer != ""} {
	    if {[catch {set remPortPointer [IBPort_p_remotePort_get $portPointer]} msg]} {
		return "$lidGuidDev port=$EntryPort"
	    } elseif { $remPortPointer == "" } {
		return "$lidGuidDev port=$EntryPort"
	    } elseif {[catch {set nodePointer [IBPort_p_node_get $remPortPointer]}]} {
		return "$lidGuidDev port=$EntryPort"
	    } elseif { $nodePointer == "" } {
		return "$lidGuidDev port=$EntryPort"
	    }
	}
    }
    if {[catch {set nodeName [IBNode_name_get $nodePointer]}]} {
	return "$lidGuidDev port=$EntryPort"
    } elseif { $nodeName == "" } {
	return "$lidGuidDev port=$EntryPort"
    } else {
	if {$addPort} {append nodeName "/P$EntryPort"}
	if { $fullName } {
	    return "\"$nodeName\" $lidGuidDev"
	} else {
	    return "$nodeName"
	}
    }
}
##############################

##############################
proc linkNamesGet { DirectPath args } {
    global G
    if {$G(bool:topology.matched)==0} { return;}

    set DirectPath [join $DirectPath]
    if { [set Port0 [lindex $DirectPath end]] == "" } {
	set Port0 $G(argv:port.num)
    }

    set PortGuid $G(data:guid.by.dr.path.[lreplace $DirectPath end end])
    set NodeGuid $G(data:NodeGuid.$PortGuid)
    if { [set Pointer(node0) \
	      [IBFabric_getNodeByGuid $G(IBfabric:.topo) $NodeGuid]] == "" } {
	return ;
    }
    set node0Ports [IBNode_Ports_get $Pointer(node0)]
    set Pointer(port0)   [lindex $node0Ports [lsearch -regexp $node0Ports "/$Port0$"]]
    catch { set Pointer(port1) [IBPort_p_remotePort_get $Pointer(port0)] }

    set linkKind "external"
    foreach I { 0 1 } {
	if { $Pointer(port${I}) == "" } { continue; }

	set Name(port${I}) [IBPort_getName $Pointer(port${I})]
	set Pointer(sysport${I}) [IBPort_p_sysPort_get $Pointer(port${I})]
	if {[BoolWordInList "-node" $args]} {
	    set Pointer(node${I}) [IBPort_p_node_get $Pointer(port${I})]
	    set Name(node${I})  [IBNode_name_get   $Pointer(node${I})]
	    lappend link "$Name(node${I})"
	} elseif { $Pointer(sysport${I}) == "" } {
	    lappend link $Name(port${I})
	    set linkKind "internal"
	} else {
	    set Pointer(node${I}) [IBPort_p_node_get $Pointer(port${I})]
	    set Num(port${I})   [IBPort_num_get    $Pointer(port${I})]
	    set Name(node${I})     [IBNode_name_get   $Pointer(node${I})]
	    lappend link "$Name(port${I})($Name(node${I})/P$Num(port${I}))"
	}
    }

    # processing the result
    switch -exact [llength $link] {
	0 {;# just to be on the safe side: if both link ends are UNKNOWN
	    return
	}
	1 {;# look for the info of the other side of the link in the
	    # "missing links" of topo matching
	    # lsearch ^ 1 = the index of the other-in-pair
	    # (note: if lsearch = -1 then index = -2)
	    # TODO: should I not report these links,
	    # as the topology matching already reported abo
	    set index [expr [lsearch -exact $G(missing.links) $link] ^ 1]
	    lappend link [lindex $G(missing.links) $index]
	}
    }
    return "names:$linkKind [list $link]"
}
##############################

##############################
# extract the name(s) of the port(s) from the -n flag
proc GetArgvPortNames {} {
    global G argv
    if { ![info exists G(argv:by-name.route)] || [CheckSkipStatus load_ibdm]} {
	return
    }
    set flag "-n"
    array set topoNodesArray [join [IBFabric_NodeByName_get $G(IBfabric:.topo)]]
    array set topoSysArray   [join [IBFabric_SystemByName_get $G(IBfabric:.topo)]]
    foreach nodeName [array names topoNodesArray] {
	foreach portPtr [join [IBNode_Ports_get $topoNodesArray($nodeName)]] {
	    set portName [IBPort_getName $portPtr]
	    set portNum  [IBPort_num_get $portPtr]
	    array set topoPortsArray   "$portName $portPtr"
	    array set topoPortsArray   "$nodeName/P${portNum} $portPtr"
	}
    }

    foreach name [split $G(argv:by-name.route) ,] {
	catch { unset portPointer portPointers }
	if {[catch { set portPointer $topoPortsArray($name) }]} {
	    if { ! [catch { set nodePointer $topoNodesArray($name) }] } {
		if { [IBNode_type_get $nodePointer] == 1 } { ; # 1=SW 2=CA 3=Rt
		    set portPointer [lindex [IBNode_Ports_get $nodePointer] 0]
		}
	    } elseif { ! [catch { set sysPointer $topoSysArray($name) }] } {
		if { [llength [set sys2node [IBSystem_NodeByName_get $sysPointer]]] == 1 } {
		    set nodePointer [lindex [join $sys2node] end]
		}
	    } else {
		inform "-E-argv:bad.node.name" -flag $flag -value "$name" \
		    -names [lsort -dictionary [array names topoNodesArray]]
	    }
	}
	if {[info exists portPointer]} {
	    if { [IBPort_p_remotePort_get $portPointer] == "" } {
		inform "-E-argv:specified.port.not.connected" \
		    -flag $flag -value "$name"
	    }
	} else {
	    if {[info exists nodePointer]} {
		set W0 "node [IBNode_name_get $nodePointer]"
		foreach pointer [IBNode_Ports_get $nodePointer] {
		    if { [IBPort_p_remotePort_get $pointer] != "" } {
			lappend portPointers $pointer
		    }
		}
	    } else {
		set W0 "system [IBSystem_name_get $sysPointer]"
		foreach sysPortNPtr [IBSystem_PortByName_get $sysPointer] {
		    set sysPointer [lindex $sysPortNPtr 1]
		    set pointer [IBSysPort_p_nodePort_get $sysPointer]
		    if { [IBPort_p_remotePort_get $pointer] != "" } {
			lappend portPointers $pointer
		    }
		}
	    }
	    if { ! [info exists portPointers] } {
		inform "-E-argv:hca.no.port.is.connected" -flag $flag -type [lindex $W0 0] -value $name
	    } elseif { [llength $portPointers] > 1 } {
		inform "-W-argv:hca.many.ports.connected" -flag $flag -type [lindex $W0 0] -value $name \
		    -port [IBPort_num_get [lindex $portPointers 0]]
	    }
	    set portPointer [lindex $portPointers 0]
	}
	lappend portNames $portPointer
    }
    return $portNames
}
##############################

##############################
proc Name2Lid {localPortPtr destPortPtr exitPort} {
    global G
    if {[CheckSkipStatus load_ibdm]} {
	return -1
    }
    set Dr $exitPort
    set listPorts $localPortPtr
    set index 0
    set Nodes($exitPort) $localPortPtr
    set destNodePtr [IBPort_p_node_get $destPortPtr]
    while { $index < [llength $Dr] } {
	set DirectPath      [lindex $Dr $index]
	set localPortPtr    $Nodes($DirectPath)
	incr index
	set localNodePtr    [IBPort_p_node_get  $localPortPtr]
	set localNodetype   [IBNode_type_get    $localNodePtr]
	set destNodePtr     [IBPort_p_node_get  $destPortPtr]

	if {$destPortPtr == $localPortPtr} {
	    if {$localNodetype == 1} {
		return "$DirectPath 0"
	    } else {
		return $DirectPath
	    }
	}
	if {($localNodetype != 1) } {continue;}
	if {(($localNodetype == 1) && ($localNodePtr == $destNodePtr))|| ($index == 1) } {
	    # in the current switch check if it's any of the switch ports
	    for {set i 1} {$i <= [IBNode_numPorts_get $localNodePtr]} {incr i} {
		set tmpPort [IBNode_getPort $localNodePtr $i]

		if {$tmpPort == $destPortPtr} {
		    return "$DirectPath 0"
		}
	    }
	}

	# build a list of new ports
	for {set i 1} {$i <= [IBNode_numPorts_get $localNodePtr]} {incr i} {
	    set tmpPort [IBNode_getPort $localNodePtr $i]
	    if {$tmpPort == ""} { continue; }
	    if { [catch {set tmpRemotePtr [IBPort_p_remotePort_get $tmpPort]} e] } {
		continue;
	    }
	    if {($tmpRemotePtr != "")} {
		if {[lsearch $listPorts $tmpRemotePtr] != -1} {continue;}
		lappend listPorts $tmpRemotePtr
		lappend Dr "$DirectPath $i"
		set newDr "$DirectPath $i"
		set Nodes($newDr) $tmpRemotePtr
	    }
	}
    }
    return -1
}
##############################

##############################
proc PathRecSelCodeText {sel} {
    switch $sel {
	0 { return ">" }
	1 { return "<" }
	2 { return "=" }
    }
    return "?"
}

proc PathRecMTUCodeText {prMTU} {
    set mtu [expr $prMTU & 0x3f]
    set sel [expr ($prMTU & 0xC0)>>6]
    switch $mtu {
	1 { set mtuStr 256 }
	2 { set mtuStr 512 }
	3 { set mtuStr 1024}
	4 { set mtuStr 2048}
	5 { set mtuStr 4096}
	default {
	    return "ERR"
	}
    }
    return "[PathRecSelCodeText $sel]$mtuStr"
}

proc PathRecRateCodeText {prRate} {
    set rate [expr $prRate & 0x3f]
    set sel [expr ($prRate & 0xC0)>>6]
    switch $rate {
	2 { set rateStr 2.5 }
	3 { set rateStr 10 }
	4 { set rateStr 30 }
	5 { set rateStr 5 }
	6 { set rateStr 20 }
	7 { set rateStr 40 }
	8 { set rateStr 60 }
	9 { set rateStr 80 }
	10 { set rateStr 120 }
	default {
	    return "ERR"
	}
    }
    return "[PathRecSelCodeText $sel]$rateStr"
}
##############################

##############################
proc DumpFabQualities {} {
    global G SM
    if {[info exists G(lst.failed)] || ![info exists G(argv:report)] || [CheckSkipStatus load_ibdm]} {
	return 1
    }

    set nodesNum [llength [array names G "data:NodeInfo.*"]]
    set swNum [llength [array names G "data:PortInfo.*:0"]]
    if { [set hcaNum [expr $nodesNum - $swNum]] == 1 } {
	inform "-W-report:one.hca.in.fabric"
	return 1
    }
    if {$G(bool:topology.matched)==1} {
	set fabric $G(IBfabric:merged)
    } else {
	set fabric $G(IBfabric:.lst)
    }

    # SM report
    set totalSM [llength [array names SM]]
    if {$totalSM != 0} {
	inform "-I-ibdiagnet:SM.header"
	DumpSMReport
    }

    inform "-I-ibdiagnet:report.fab.qualities.header"

    # general reports
    if {[IBFabric_parseFdbFile $fabric $G(outfiles,.fdbs)]} {
	inform "-F-crash:failed.parse.fdbs"
    }

    if {[IBFabric_parseMCFdbFile $fabric $G(outfiles,.mcfdbs)]} {
	inform "-F-crash:failed.parse.mcfdbs"
    }

    if {![file exists $G(outfiles,.lst)]} {
	inform "-E-ibdiagnet:no.lst.file" -fileName $G(outfiles,.lst)
	return 1
    }

    # verifying CA to CA routes
    ibdmUseInternalLog
    ibdmVerifyCAtoCARoutes $fabric
    ibdmCheckMulticastGroups $fabric
    set report [ibdmGetAndClearInternalLog]
    inform "-I-ibdiagnet:report.fab.qualities.report" $report
    set nErrs [regexp -all -- {-E-} $report]
    if {$nErrs} {
	inform "-E-ibdiagnet:report.fab.qualities.errors" $nErrs
    }
    set nWarns [regexp -all -- {-W-} $report]
    if {$nWarns} {
	inform "-W-ibdiagnet:report.fab.qualities.warnings" $nWarns
    }

    inform "-I-ibdiagnet:check.credit.loops.header"

    # report credit loops
    ibdmCalcMinHopTables $fabric
    set roots [ibdmFindRootNodesByMinHop $fabric]
    # just flush out any logs
    set report [ibdmGetAndClearInternalLog]
    if {[llength $roots]} {
	inform "-I-reporting:found.roots" $roots
	ibdmReportNonUpDownCa2CaPaths $fabric $roots
    } else {
	ibdmAnalyzeLoops $fabric
    }
    set report [ibdmGetAndClearInternalLog]
    inform "-I-ibdiagnet:report.fab.credit.loop.report" $report

    set nErrs [regexp -all -- {-E-} $report]
    if {$nErrs} {
	inform "-E-ibdiagnet:report.fab.credit.loop.errors" $nErrs
    }
    set nWarns [regexp -all -- {-W-} $report]
    if {$nWarns} {
	inform "-W-ibdiagnet:report.fab.credit.loop.warnings" $nWarns
    }

    # back to send ibdm messages to cout
    ibdmUseCoutLog

    # Multicast mlid-guid-hcas report
    set mcPtrList [sacMCMQuery getTable 0]

    if { [llength $mcPtrList] > 0 } {
	inform "-I-ibdiagnet:mgid.mlid.hca.header"
	set mcgFile [InitializeOutputFile $G(var:tool.name).mcgs]
	set preGuid ""
	puts "mgid [Bar " " 32] | mlid   | PKey   | QKey       | MTU   | rate     | HCAs"
	foreach mcPtr $mcPtrList {
	    if {[catch {sacMCMRec OBJECT -this $mcPtr} msg]} {
		puts $msg
	    } else {
		catch {OBJECT cget} attributes
		foreach attr [lindex $attributes 0] {
		    set [string range $attr 1 end] [OBJECT cget $attr]
		}
		rename OBJECT ""
	    }
	    set mlidHex 0x[format %lx $mlid]
	    if {[info exists G(mclid2DrPath,$mlidHex)]} {
		set mlidHcas $G(mclid2DrPath,$mlidHex)
	    } else {
		set mlidHcas NONE
	    }
	    set mtuStr [PathRecMTUCodeText $mtu]
	    set rateStr "[PathRecRateCodeText $rate]Gbps"
	    set msg "$mgid | [format 0x%lx $mlid] | [format 0x%04x $pkey] | [format 0x%08x $qkey] | [format %-5s $mtuStr] | [format %-8s $rateStr] | [llength $mlidHcas]"
	    puts $msg
	    puts $mcgFile "GROUP: $msg"
	    foreach hca $mlidHcas {
		puts $mcgFile "  $hca"
	    }
	}
	close $mcgFile
    }

    return 0
}
######################################################################

######################################################################
### format fabric info
######################################################################
# The pocedure GetParamValue needs the database $G(data:list.direct.path)
# returns the value of a parameter of a port in .lst file format

##############################
proc GetDeviceFullType {_name} {
    array set deviceNames { SW "Switch" CA "HCA" Rt "Router" }
    if {[lsearch [array names deviceNames] $_name] == -1} {
	return $_name
    } else {
	return $deviceNames($_name)
    }
}
##############################

##############################
proc GetEntryPort { _directPath args} {
    global G INFO_LST Neighbor
    if {$_directPath == ""} {
        if {[lsearch -exac $args "-byNodeInfo"]!=-1} {
	    set nodeInfo [GetWordAfterFlag $args "-byNodeInfo"]
	} else {
	    set nodeInfo [SmMadGetByDr NodeInfo dump ""]
	}
	set _port_num_vendor_id [GetWordAfterFlag $nodeInfo "-port_num_vendor_id"]
        return [format %d [FormatInfo $_port_num_vendor_id PortNum NONE]]
    }

    if {[info exists G(data:guid.by.dr.path.$_directPath)]} {
        set tmpGuid $G(data:guid.by.dr.path.[lrange $_directPath 0 end-1])
	set tmpGuid $G(data:NodeGuid.$tmpGuid)
	if {[info exists Neighbor($tmpGuid:[lindex $_directPath end])]} {
	    set entryPort $Neighbor($tmpGuid:[lindex $_directPath end])
            return [lindex [split $entryPort :] end ]
	}
    }

    if {[lsearch -exac $args "-byNodeInfo"]!=-1} {
	set nodeInfo [GetWordAfterFlag $args "-byNodeInfo"]
	set _port_num_vendor_id [GetWordAfterFlag $nodeInfo "-port_num_vendor_id"]
        return [format %d [FormatInfo $_port_num_vendor_id PortNum NONE]]
    } elseif {$_directPath == ""} {
        return -code 1 -errorinfo "Can't retrive entry port"
    }

    if {[catch {set tmpGuid [GetParamValue NodeGUID [lrange $_directPath 0 end-1] -byDr]}]} {
        return ""
    } else {
	if {[info exists Neighbor($tmpGuid:[lindex $_directPath end])]} {
	    set entryPort $Neighbor($tmpGuid:[lindex $_directPath end])
	    return [lindex [split $entryPort :] end ]
	} else {
            return ""
	}
    }
}
##############################

##############################
proc GetParamValue { parameter DirectPath args } {
    global G INFO_LST
    set DirectPath "[join $DirectPath]"
    # noread - if info doesn't exists don't try to get it by dr
    set byDr 0  
    set noread 0
    if {[lsearch -exac $args "-byDr"] != -1} { set byDr 1 }
    if {[lsearch -exac $args "-noread"] != -1} { set noread 1}
    if {[BoolWordInList $parameter "PortGuid"]} { set byDr 1 }
    if { ! [BoolWordInList $DirectPath $G(data:list.direct.path)] && (![BoolWordInList $DirectPath $G(data:list.bad.paths)]) && (!$byDr)} {
	return -code 1 -errorinfo "Direct Path \"$DirectPath\" not in $G(data:list.direct.path)\n and not in $G(data:list.bad.paths)"
    }
    ## Setting the parameter flags
    ParseOptionsList $INFO_LST($parameter)

    ## Setting the port number
    if {[lsearch -exac $args "-port"] != -1} {
	set port [GetWordAfterFlag $args "-port"]
    }
    if {[info exists cfg(fromport0)]} {
	if {$byDr} {
	    if { [catch {set tmpType [GetParamValue Type $DirectPath -byDr] }]} {
		return -code 1 -errorinfo "6.Direct Path \"$DirectPath\" is bad"
	    }
	} else {
	    if { [catch {set tmpType [GetParamValue Type $DirectPath] }]} {
		return -code 1 -errorinfo "6.Direct Path \"$DirectPath\" is bad"
	    }
	}
	if {$tmpType == "SW" }  {
	    set port 0
	}
    }

    # Check if the provided port is legal (bigger then zero, less or equal to Ports)
    if {[info exists port]} {
	if {$port < 0} {
	    return -code 1 -errorinfo "Node at end of Direct Path \"$DirectPath\" Does not have port $port"
	}
	if {$byDr} {
	    if { [catch {set maxPorts [GetParamValue Ports $DirectPath -byDr] }]} {
		return -code 1 -errorinfo "6.Direct Path \"$DirectPath\" is bad"
	    }
	} else {
	    if { [catch {set maxPorts [GetParamValue Ports $DirectPath] }]} {
		return -code 1 -errorinfo "6.Direct Path \"$DirectPath\" is bad"
	    }
	}
	if {$port > $maxPorts} {
	    return -code 1 -errorinfo "Node at end of Direct Path \"$DirectPath\" Does not have port $port"
	}
    }

    ## setting port/node guids
    if {[info exists G(data:guid.by.dr.path.$DirectPath)]} {
	set PortGuid $G(data:guid.by.dr.path.$DirectPath)
	if {[info exists G(data:NodeGuid.$PortGuid)]} {
	    set NodeGuid $G(data:NodeGuid.$PortGuid)
	} else {
	    set byDr 1
	}
    } else {
	set byDr 1
    }
    ### Getting the parameter value
    set value "DannyZarko"
    switch -exact -- $parameter {
	"PN" { return [FormatInfo $port PN $DirectPath] }
	"PortGUID" {
	    set addPort2Cmd [regexp {(Port|Lft)} $cfg(source)]
	    if {[info exists PortGuid]} {
		return [FormatInfo $PortGuid $parameter $DirectPath]
	    } else {
		set Cmd [list SmMadGetByDr $cfg(source) -$cfg(flag) "$DirectPath"]
		if {$addPort2Cmd} { append Cmd " $port" }
		if {[catch { set value [eval $Cmd]}]} { return -code 1 }
	    }
	}
	default {
	    set addPort2Cmd [regexp {(Port|Lft)} $cfg(source)]
	    if {[info exists NodeGuid]} {
		set InfoSource "data:$cfg(source).$NodeGuid"
		if {$addPort2Cmd} { append InfoSource ":$port" }
	    } else {
		set InfoSource "DannyZarko"
	    }
	    if {$byDr} {
		if {$noread} { return -code 1 -errorinfo "1.Direct Path \"$DirectPath\" is bad"}
		if { [PathIsBad $DirectPath] > 1 } {
		    return -code 1 -errorinfo "2.Direct Path \"$DirectPath\" is bad"
		}
		set Cmd [list SmMadGetByDr $cfg(source) -$cfg(flag) "$DirectPath"]
		if {$addPort2Cmd} { append Cmd " $port" }
		if {[catch { set value [eval $Cmd]}]} { return -code 1 -errorinfo "5.Direct Path \"$DirectPath\" is bad"}
	    } else {
		if {[info exists G($InfoSource)]} {
		    if {$parameter == "NodeDesc"} {
			return [FormatInfo $G(data:NodeDesc.$NodeGuid) NodeDesc $DirectPath]
		    }
		    return [FormatInfo [GetWordAfterFlag $G($InfoSource) -$cfg(flag)] $parameter $DirectPath]
		} else {
		    if { [PathIsBad $DirectPath] > 1 } {
			return -code 1 -errorinfo "3.Direct Path \"$DirectPath\" is bad"
		    }
		    if {$noread} { return -code 1 -errorinfo "4.DannyZarko Direct Path \"$DirectPath\" is bad"}
		    set Cmd [list SmMadGetByDr $cfg(source) -$cfg(flag) "$DirectPath"]
		    if {$addPort2Cmd} { append Cmd " $port" }
		    if {[catch { set value [eval $Cmd]}]} { return -code 1 }
		}
	    }
	}
    }
    return [FormatInfo $value $parameter $DirectPath]
}
##############################

##############################
proc FormatInfo {_value _parameter _directRoute} {
    global G INFO_LST MASK
    set value $_value
    ParseOptionsList $INFO_LST($_parameter)
    ## Formatting $value
    catch { set value [format %lx $value] }
    regsub {^0x} $value {} value

    # bits -> bytes
    if {[catch { set width [expr $cfg(width) / 4] }]} { set width "" }

    if {!(( $width == 0 ) || ( ! [regexp {^[0-9]+} $width] )) } {
	if {[info exists cfg(offset)]} {
	    scan $cfg(offset) {%d%[:]%d} offset D bigwidth
	    set bigwidth [expr $bigwidth / 4]
	    set offset [expr $offset / 4]
	    set value [AddZeroes $value $bigwidth]
	    set value [string range $value $offset [expr $offset + $width -1]]
	} else {
	    set value [AddZeroes $value $width]
	}
    }

    if {[info exists cfg(substitution)]} {
	regsub -all { *= *} [join $cfg(substitution)] {= } substitution
	set value [RemoveZeroes $value]
	set value [GetWordAfterFlag $substitution "$value="]
    }
    if { ! [info exists cfg(string)] } {
	set value "0x$value"
    }
    return $value
}
##############################

######################################################################
### ouput fabric info
######################################################################
proc linkAtPathEnd { Path } {
    if { [catch { set port1 [GetEntryPort $Path] } ] } {
	return -code 1
    }

    uplevel  1 set path0 \"[lreplace $Path end end]\"
    uplevel  1 set port0 [lindex $Path end]
    uplevel  1 set path1 \"$Path\"
    uplevel  1 set port1 $port1
}
##############################

##############################
#  NAME         lstInfo
#  SYNOPSIS     lstInfo type{port|link} DirectPath port
#  FUNCTION     returns either the info of one of a port in .lst format
#                       or the info regarding the links : SPD,PHY,LOG
#  INPUTS       NULL
#  OUTPUT       returns the info of one of a port in .lst format
proc lstInfo { type DirectPath port } {
    global G MASK SM
    set DirectPath [join $DirectPath]
    set Info ""
    ## The lists of parameters
    switch -exact -- $type {
	"port" {
	    set sep ":"
	    append lstItems "Type Ports SystemGUID NodeGUID PortGUID VenID"
	    append lstItems " DevID Rev NodeDesc LID PN"
	} "link" {
	    set sep "="
	    append lstItems "PHY LOG SPD"
	}
    }

    foreach parameter $lstItems {
	# The following may fail - then the procedure will return with error
	# Known Issue - GetParamValue will return
	regsub {^0x} [GetParamValue $parameter $DirectPath -port $port] {} value
	# .lst formatting of parameters and their values
	if {[BoolWordInList $parameter "VenID DevID Rev LID PN"]} {
	    set value [string toupper $value]
	}
	switch -exact -- $parameter {
	    "Ports"     { set tmpPorts  $value }
	    "PN"        { set tmpPN     $value }
	}
	switch -exact -- $parameter {
	    "Type"   {
		# Replace CA with CA-SM
		if {$value == "CA"} {
		    set master 3
		    if {[info exists SM($master)]} {
			foreach element $SM($master) {
			    set tmpDirectPath [lindex $element 0]
			    if {$DirectPath == $tmpDirectPath} {
				set value "CA-SM"
			    }
			}
		    }
		}
		lappend Info "$value"
	    }
	    "SystemGUID" {
		# use node guid instead for zero system image guid
		if {$value == "0x0000000000000000"} {
		    regsub {^0x} [GetParamValue NodeGUID $DirectPath -port $port] {} value
		}
		lappend Info "${parameter}${sep}${value}"
	    }
	    "NodeDesc"  { lappend Info "\{$value\}" }
	    "DevID"  { lappend Info "${parameter}${sep}${value}0000" }
	    "VenID"  { lappend Info "${parameter}${sep}00${value}" }
	    default  { lappend Info "${parameter}${sep}${value}" }
	}
    }
    if {$type == "port"} {
	if {[info exists tmpPorts] && [info exists tmpPN]} {
	    if {$tmpPorts < $tmpPN} {
		set G(lst.failed) 1
	    }
	}
    }
    return [join $Info]
}
##############################

##############################
#  NAME         writeDBFile
#  SYNOPSIS     writeDBFile
#  FUNCTION     writes a dump of the G Array
#  INPUTS       NULL
#  OUTPUT       NULL
proc writeDBFile {} {
    global G Neighbor

    if {[info exists G(argv:ibdiag.db)]} {
        return 1
    }

    set FileID [InitializeOutputFile $G(var:tool.name).db]

    foreach {array_name data} {G data* Neighbor *} {
        puts $FileID "array set $array_name {"

        set tmp_list [array get $array_name $data]
        foreach {key val} $tmp_list {
            set line "{$key} {$val}"
            puts $FileID $line
        }
        puts $FileID "}"
    }

    close $FileID
    return 0
}
##############################

##############################
#  NAME         writeLstFile
#  SYNOPSIS     writeLstFile
#  FUNCTION     writes a dump of the fabric links
#  INPUTS       NULL
#  OUTPUT       NULL
proc writeLstFile {} {
    global G

    set FileID [InitializeOutputFile $G(var:tool.name).lst]
    foreach DirectPath $G(data:list.direct.path) {
	# seperate the next 3 logical expr to avoid extra work
	if {![llength $DirectPath]  } {continue; }
	if {[PathIsBad $DirectPath] > 1 } {continue; }
	if {[catch {linkAtPathEnd $DirectPath}] } {continue; }
	set lstLine ""
	append lstLine "\{ [lstInfo port $path0 $port0] \} "
	append lstLine "\{ [lstInfo port $path1 $port1] \} "
	append lstLine "[lstInfo link $path0 $port0]"
	puts $FileID "$lstLine"
	unset path0
	unset path1
	unset port0
	unset port1
    }
    close $FileID
    return 0
}
##############################

##############################
#  NAME         writeNeighborFile
#  SYNOPSIS     writeNeighborFile
#  FUNCTION     writes a dump of the ports pairs in the discovered fabric
#  INPUTS       NULL
#  OUTPUT       NULL
proc writeNeighborFile { args } {
    global Neighbor G

    set FileID [InitializeOutputFile $G(var:tool.name).neighbor]
    set preGuid ""
    foreach neighbor [lsort -dictionary [array names Neighbor]] {
	if {($preGuid != [string range $neighbor 0 17]) && ($preGuid != "")} {
	    puts $FileID ""
	}
	puts $FileID "$neighbor\t$Neighbor($neighbor)"
	set preGuid [string range $neighbor 0 17]
    }
    close $FileID
    return 0
}
##############################

##############################
#  NAME         writeMasksFile
#  SYNOPSIS     writeMasksFile
#  FUNCTION     writes a map for duplicate GUIDs <-> New assgiened GUIDs
#  INPUTS       NULL
#  OUTPUT       NULL
proc writeMasksFile { args } {
    global MASK G
    if {[llength [array names MASK *Guid,*]] == 0 } {
	return 0
    }
    set FileID [InitializeOutputFile $G(var:tool.name).masks]
    foreach mask [lsort -dictionary [array names MASK *Guid,*]] {
	puts $FileID "$mask\t$MASK($mask)"
    }
    close $FileID
    return 0
}
##############################

##############################
#  NAME         writeSMFile
#  SYNOPSIS     writeSMFile
#  FUNCTION     writes a dump of SM query
#  INPUTS       NULL
#  OUTPUT       NULL
proc writeSMFile {} {
    global SM G
    set SMFound 0
    for {set i 3} {$i > -1} {incr i -1} {
	if {[info exists SM($i)]} {
	    set SMFound 1
	}
    }

    if {!$SMFound} {return 0}
    set FileID [InitializeOutputFile $G(var:tool.name).sm]

    puts $FileID "ibdiagnet fabric SM report"

    DumpSMReport $FileID
    close $FileID
    return 0
}

##############################
#  NAME         writePMFile
#  SYNOPSIS     writePMFile
#  FUNCTION     writes a dump of Port Counter query
#  INPUTS       NULL
#  OUTPUT       NULL
proc writePMFile {} {
    global G PM_DUMP
    if {![info exists PM_DUMP]} {return 0}
    set FileID [InitializeOutputFile $G(var:tool.name).pm]
    foreach name $PM_DUMP(nodeNames) {
	puts $FileID [string repeat "-" 80]
	puts $FileID $name
	puts $FileID [string repeat "-" 80]
	set tmpPmCounterList $PM_DUMP($name,pmCounterList)
	set listOfPMValues $PM_DUMP($name,pmCounterValue)
	foreach pmCounter $tmpPmCounterList {
	    set pmCounterValue "0x[format %lx [GetWordAfterFlag $listOfPMValues $pmCounter]]"
	    puts $FileID "$pmCounter = $pmCounterValue"
	}
    }

    close $FileID
    return 0
}

##############################

##############################
#  NAME         writeTopologyFileAndIBNLs
#  SYNOPSIS     writeTopologyFileAndIBNLs
#  FUNCTION     Write out teh topoly fiel if requested
#  INPUTS       NULL
#  OUTPUT       NULL
proc writeTopologyFileAndIBNLs {} {
    global G
    # we might be asked to dump out the topology
    if {[info exists G(argv:write.topology)]} {
	inform "-I-write.topology:writing"
	set ibnlDir [file join $G(argv:out.dir) ibdiag_ibnl]
	if {![file exists $ibnlDir]} {
	    file mkdir $ibnlDir
	}
	
	# we might be teh only reason to get a topology ...
	if {![info exists G(IBfabric:merged)] } {
	    set f [new_IBFabric]
	    if {[IBFabric_parseSubnetLinks $f  $G(outfiles,.lst)]} {
		inform "-F-crash:failed.parse.lst"
	    }
	} else {
	    set f $G(IBfabric:merged)
	}
	if {[IBFabric_dumpTopology $f $G(argv:write.topology) $ibnlDir]} {
	    inform "-E-write.topology:failed"
	}
    }
    return 0
}

##############################
#  SYNOPSIS write.fdbsFile
#  FUNCTION
#  writes the $G(var:tool.name).fdbs file, which lists the Linear Forwarding Tables
#  of all the switches in the discovered faric.
#  Writing this file is part of the flow of ibdiagnet.
#  The data is obtained by sending LftBlock MADs to read all the entires
#  of the Linear Forwarding Tables to all the switches.
#  The file has the following format for each switch of the IB fabric:
#     Switch <NodeGuid>
#     LID    : Out Port(s)
#     0xc000   0x002 0x00f
#     ...
#  INPUTS   NULL
#  OUTPUT   the file $G(var:tool.name).mcfdbs
#  DATAMODEL
#  the procedure uses the global arrays
#  $G(data:PortInfo.<NodeGuid>:0) - as a list of all the switches
#  and $G(Guid2DrPath,<NodeGuid>) - to translate node-guids to direct paths
#  it sets the global array $G(mclid2DrPath,<mcLid>) - a list of (direct
#  paths to) HCAs belonging to a multicast-lid - to be used later by
#  DumpFabQualities.
proc writeFdbsFile { args } {
    global G

    set FileID [InitializeOutputFile $G(var:tool.name).fdbs]
    foreach entry [array names G "data:dr.path.to.guid.*"] {

	set DirectPath $G($entry)
	if {[PathIsBad $DirectPath] > 1} { continue; }
	set NodeType [GetParamValue Type $G($entry)]
	if {$NodeType != "SW"} { continue; }

	set PortGuid [lindex [split $entry .] end]
	set NodeGuid $G(data:NodeGuid.$PortGuid)

	set thisSwLid [GetParamValue LID $DirectPath X -noread]
	if {[PathIsBad $DirectPath] > 1} { continue; }
	if [catch {set LinFDBTop \
		       [SmMadGetByDr SwitchInfo -lin_top "$DirectPath"]}] {
	    continue;
	}
	set FDBs ""
	for { set I 0 } { [expr $I *64] <= $LinFDBTop } { incr I } {
	    # Note "<=" - because LinFDBTop indicates the INDEX of the last
	    # valid entry
	    if [catch {set NewFDBs \
			   [SmMadGetByDr LftBlock dump "$DirectPath" $I] }] {
		set FDBs [concat $FDBs [Bar "0xff " 64]]
	    } else {
		set FDBs [concat $FDBs $NewFDBs]
	    }
	}
	puts -nonewline $FileID "osm_ucast_mgr_dump_ucast_routes: "
	puts $FileID "Switch $NodeGuid"
	puts $FileID "LID    : Port : Hops : Optimal"
	for { set lid 1 } { $lid <= $LinFDBTop } { incr lid 1 } {
	    scan [lindex $FDBs $lid] %x port
	    puts -nonewline $FileID "0x[string toupper [format %04x $lid]] : "
	    if { $port == "0xff" } {
		puts $FileID "UNREACHABLE"
	    } elseif { ( $port == "0x00" ) && ( $lid != $thisSwLid ) } {
		puts $FileID "UNREACHABLE"
	    } else {
		puts $FileID "[AddZeroes $port 3]  : 00   : yes"
	    }
	}
	puts $FileID ""
    }
    close $FileID
    return 0
}
##############################

##############################
#  SYNOPSIS write.mcfdbsFile
#  FUNCTION
#  writes the $G(var:tool.name).mcfdbs file, which lists the Multicast Forwarding
#  Tables of all the switches in the discovered faric.
#  Writing this file is part of the flow of ibdiagnet.
#  The data is obtained by sending MftBlock MADs to read all the entires
#  of the MC Forwarding Tables. Note the tables are read in blocks of
#  16 ports x 64 mcLids blocks, thus if a deviec has more than 16 ports
#  then reading its mc table is a bit tricky...
#  The file has the following format for each switch of the IB fabric:
#     Switch <NodeGuid>
#     LID    : Out Port(s)
#     0xc000   0x002 0x00f
#     ...
#  INPUTS   NULL
#  OUTPUT   the file $G(var:tool.name).mcfdbs
#  DATAMODEL
#  the procedure uses the global arrays
#  $G(data:PortInfo.<NodeGuid>:0) - as a list of all the switches
#  and $G(Guid2DrPath,<NodeGuid>) - to translate node-guids to direct paths
#  it sets the global array $G(mclid2DrPath,<mcLid>) - a list of (direct
#  paths to) HCAs belonging to a multicast-lid - to be used later by
#  DumpFabQualities.
proc writeMcfdbsFile { } {
    global G

    set FileID [InitializeOutputFile $G(var:tool.name).mcfdbs]

    foreach entry [array names G "data:dr.path.to.guid.*"] {
	set DirectPath $G($entry)
	if {[PathIsBad $DirectPath] > 1} {
	    continue;
	}
	set NodeType [GetParamValue Type $G($entry)]
	if {$NodeType != "SW"} { continue; }
	set PortGuid [lindex [split $entry .] end]
	set NodeGuid $G(data:NodeGuid.$PortGuid)

	if {[catch { set McFDBCap [SmMadGetByDr SwitchInfo -mcast_cap "$DirectPath"] }]} {
	    continue;
	}
	set NumPorts [GetParamValue Ports $DirectPath]
	puts $FileID "\nSwitch $NodeGuid\nLID    : Out Port(s) "
	for {set LidGrp 0xc000} {$LidGrp < 0xc000 + $McFDBCap} {incr LidGrp 0x20} {
	    set McFDBs ""
	    set LidGroup "0x[format %lx $LidGrp]"
	    # read the entire McFDBs data for Lids $LidGroup .. $LidGroup + 0x1f
	    for {set PortGroup 0} {$PortGroup <= $NumPorts} {incr PortGroup 16} {
		if {[catch { set newBlock [SmMadGetByDr MftBlock dump "$DirectPath" $LidGroup $PortGroup] }]} { break; }
		if {[lindex $newBlock 0] == "-mft"} {
		    append McFDBs " " [Hex2Bin [lrange $newBlock 1 end]]
		} else {
		    append McFDBs " " [Hex2Bin $newBlock]
		}
	    }
	    # figure out - and print to file - the mc ports for each Lid
	    # in the lid group
	    for { set lidIndx 0 } { $lidIndx < 0x20 } { incr lidIndx } {
		set mask ""
		for { set PortGroup 0; set idx 0 } { $PortGroup <= $NumPorts } { incr PortGroup 16; incr idx 32 } {
		    set mask "[lindex $McFDBs [expr $lidIndx + $idx]]$mask"
		}
		if { ! [regexp "1" $mask] } { continue; }
		set mcLid [format %04x [expr $lidIndx + $LidGroup]]
		set outputLine "0x[string toupper $mcLid] :"
		for { set Port 0; set maskIndx [expr [string length $mask]-1] } { $Port <= $NumPorts } { incr Port 1 ; incr maskIndx -1 } {
		    if { [string index $mask $maskIndx] == 1 } {
			append outputLine " 0x[string toupper [format %03x $Port]] "
			set LongPath [join "$DirectPath $Port"]
			set tmp_badLinksDetectionValue $G(bool:bad.links.detected)
			set G(bool:bad.links.detected) 0
			if {[catch {SmMadGetByDr NodeInfo dump $LongPath} e]} {
			    set G(bool:bad.links.detected) $tmp_badLinksDetectionValue
			    continue;
			}
			set G(bool:bad.links.detected) $tmp_badLinksDetectionValue
			catch { if { [GetParamValue Type $LongPath -byDr] != "SW" } {
			    set directPathName [DrPath2Name $LongPath -byDr]
			    if {$directPathName !=""} {
				lappend G(mclid2DrPath,0x$mcLid) $directPathName
			    } else {
				lappend G(mclid2DrPath,0x$mcLid) $LongPath
			    }
			}
			}
		    }
		}
		puts $FileID "$outputLine"
	    }
	}
    }
    close $FileID
    return 0
}
######################################################################

proc CheckAllinksSettings {} {
    global G LINK_SPD LINK_PHY
    set checkList ""
    set spd ""
    set phy ""
    if {[info exists G(argv:link.width)]} {
	lappend checkList "PHY"
	set phy $G(argv:link.width)
    }
    if {[info exists G(argv:link.speed)]} {
	lappend checkList "SPD"
	set spd $G(argv:link.speed)
    }

    foreach DirectPath $G(data:list.direct.path) {
	if {$DirectPath == ""} {
	    continue;
	}
	if {[lsearch $checkList "SPD"] != -1} {
	    set tmpLinkspeed [GetParamValue "SPD" $DirectPath -port [GetEntryPort $DirectPath]]
	    if {$tmpLinkspeed != $spd} {
		lappend LINK_SPD($DirectPath) $tmpLinkspeed
	    }
	}
	if {[lsearch $checkList "PHY"] != -1} {
	    set tmpLinkWidth [GetParamValue "PHY" $DirectPath -port [GetEntryPort $DirectPath]]
	    if {$tmpLinkWidth != $phy} {
		lappend LINK_PHY($DirectPath) $tmpLinkWidth
	    }
	}
    }

    if {[lsearch $checkList "PHY"] != -1} {
	inform "-I-ibdiagnet:bad.link.width.header"
	if {[llength [array names LINK_PHY]]} {
	    foreach link [lsort [array names LINK_PHY]] {                                                   
		if {[PathIsBad $link] > 1} {continue;}
		set paramlist "-DirectPath0 \{[lrange $link 0 end-1]\} -DirectPath1 \{$link\}"
		eval inform "-W-ibdiagnet:report.links.width.state" -phy $LINK_PHY($link) $paramlist
	    }
	} else {
	    inform "-I-ibdiagnet:no.bad.link.width"
	}
    }

    if {[lsearch $checkList "SPD"] != -1} {
	inform "-I-ibdiagnet:bad.link.speed.header"
	if {[llength [array names LINK_SPD]]} {
	    foreach link [lsort [array names LINK_SPD]] {                                                   
		if {[PathIsBad $link] > 1} {continue;}
		set paramlist "-DirectPath0 \{[lrange $link 0 end-1]\} -DirectPath1 \{$link\}"
		eval inform "-W-ibdiagnet:report.links.speed.state" -spd $LINK_SPD($link) $paramlist
	    }
	} else {
	    inform "-I-ibdiagnet:no.bad.link.speed"
	}
    }
    return 0
}

proc SL_2_VL {_paths _targets} {
    global G
    for {set i 0} {$i < 16} {incr i} {
	set SL_VL($i,VL) -1
    }
    set path [lindex $_paths end]
    for {set i 0} {$i < [llength $path]} {incr i} {
	set tmpPath [lrange $path 0 $i]
	set smallPath [lreplace $tmpPath end end ]
	set entryPort [GetParamValue PortNum $tmpPath -byDr]

	set inPort [GetParamValue PortNum $smallPath -byDr]
	set outPort [lindex $tmpPath end]
	set slVlTable [SmMadGetByDr SlVlTable dump "$smallPath" $inPort $outPort]
	set slVlString ""
	foreach item $slVlTable  {
	    append slVlString [string range $item 2 end]
	}
	set tmpOpVL [GetParamValue OpVL "$tmpPath" -port $entryPort -byDr]
	set slVlString $slVlString
	for {set j 0} {$j < 16} {incr j} {
	    if {$SL_VL($j,VL) != -1} { continue}
	    if {$tmpOpVL < [string index $slVlString $j]} {
		set SL_VL($j,VL) [string index $slVlString $j]
		set SL_VL($j,directPath) $tmpPath
		set SL_VL($j,opVL) $tmpOpVL
	    }
	}
    }

    for {set i 0} {$i < 16} {incr i} {
	if {$SL_VL($i,VL) == -1} {
	    lappend suitableSl $i
	}
    }

    set directPath ""
    set opVL ""
    set VL -1
    if {[info exists G(argv:lid.route)]} {
	set lidRoute ""
	foreach lid [split $G(argv:lid.route) ,] {
	    lappend lidRoute "0x[format %x $lid]"
	}
	if {[llength $_targets] > 1} {
	    set route "path From lid: [lindex $lidRoute 0] To lid: [lindex $lidRoute 1]"
	} else {
	    set route "path From lid: $G(data:root.port.lid) To lid: [lindex $lidRoute 0]"
	}
    }
    if {[info exists G(argv:by-name.route)]} {
	set nameRoute [split $G(argv:by-name.route) ,]
	if {[llength $_targets] > 1} {
	    set route "path From: [lindex $nameRoute 0] To: [lindex $nameRoute 1]"
	} else {
	    set route "path From: [DrPath2Name ""] To: [lindex $nameRoute 0]"
	}
    }

    if {[info exists G(argv:direct.route)]} {
	set route "direct route: $G(argv:direct.route)"
    }

    if {[info exists G(argv:service.level)]} {
	if {$SL_VL($G(argv:service.level),VL) != -1 } {
	    set directPath "$SL_VL($G(argv:service.level),directPath)"
	    set opVL [Hex2Dec $SL_VL($G(argv:service.level),opVL)]
	    set VL [Hex2Dec $SL_VL($G(argv:service.level),VL)]
	}
    }

    inform "-I-ibdiagpath:service.level.header"
    inform "-I-ibdiagpath:service.level.report" -suitableSl $suitableSl -DirectPath0 $directPath -DirectPath1 [lreplace  $directPath end end] -opVL $opVL -VL $VL -route $route
    return 0
}

proc Hex2Dec {hexaNum} {
    scan $hexaNum "%x" dec
    return $dec
}

proc CheckSkipStatus {_step} {
    global G
    if {[BoolWordInList $_step $G(argv:skip.checks)]} {
        return 1
    }
    if {$_step == "load_ibdm"} {
        return 0
    }
    if {[BoolWordInList all $G(argv:skip.checks)]} {
        return 1
    }
    return 0
}
