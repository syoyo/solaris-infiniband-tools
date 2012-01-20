######################################################################
### Procs:
# PutsIn80Chars
# toolsFlags
# ParseArgv
# inform
# showHelpPage
######################################################################

######################################################################
## varoius definitions for the command-line arguments:
# name:     the name to be used when refering to the flag (e.g., topo.file for -t)
# default:  the default value (if exists)
# param:    a string describing the value given to the flag - to be written in the tool's synopsys
#           (e.g., the "count" in: [-c <count>])
# -deafult  "" means that the parameter does have a default value, but it will set later
#           (after ibis is ran, in procedure StartIBDIAG).
## TODO: sm_key is a 64-bit integer - will it be correctly cheked in ParseArgv ?
proc SetInfoArgv {} {
   global InfoArgv G
   array set InfoArgv {
      -load_db,name     "ibdiag.db"
      -load_db,desc     "Load subnet data from the given .db file. Skip subnet discovery stage"
      -load_db,param    "db_file"
      -load_db,arglen   "1"
      -load_db,regexp   "file.exists.readable"

      -skip,name     "skip.checks"
      -skip,desc     "Skip the executions of part of the application"
      -skip,param    "ibdiag_check/s"
      -skip,error    "-E-argv:not.legal.skip"
      -skip,default  "None"
      -skip,arglen   "1.."

      -smp,name   "symmetric.multi.processing"
      -smp,desc   "Instructs the tool to run in smp mode"
      -smp,arglen 0
      -smp,default 0

      -sl,name    "service.level"
      -sl,desc    "Determine if the provided sl is legit for the route"
      -sl,param   "service level"
      -sl,regexp  "integer.nonneg.==1"
      -sl,error   "-E-argv:not.pos.integer"
      -sl,maxvalue "15"

      -P,name     "query.performance.monitors"
      -P,desc     "If any of the provided pm is greater then its provided value, print it to screen"
      -P,param "<PM>=<Value>"
      -P,error "-E-argv:not.legal.PM"
      -P,arglen   "1.."

      -pc,name    "reset.performance.monitors"
      -pc,desc    "reset all the fabric links pmCounters"
      -pc,arglen  0

      -pm,name    "performance.monitors"
      -pm,desc "Dumps all pmCounters values into .pm file"
      -pm,arglen  0

      -wt,name    "write.topology"
      -wt,desc "Write out the discovered topology into the given file name. Required IBNL files are created in the ibdiag_ibnl dir."
      -wt,arglen  1

      -lw,name    "link.width"
      -lw,param   "1x|4x|12x"
      -lw,error   "-E-argv:not.legal.link.width"
      -lw,regexp  {^((1x)|(4x)|(12x))$}
      -lw,desc    "Specifies the expected link width"

      -ls,name    "link.speed"
      -ls,param   "2.5|5|10"
      -ls,error   "-E-argv:not.legal.link.speed"
      -ls,regexp  {^((2\.5)|(5)|(10))$}
      -ls,desc    "Specifies the expected link speed"

      -a,name  "attribute"
      -a,param "attr"
      -a,optional "-field1 val1 -field2 val2 ..."
      -a,desc  "defines the attribute to query or send"

      -c,name  "count"
      -c,default  10
      -c,param "count"
      -c,regexp   "integer.nonneg.==1"
      -c,error "-E-argv:not.nonneg.integer"
      -c,maxvalue "1000000"

      -d,name  "direct.route"
      -d,param "p1,p2,p3,..."
      -d,desc  "Directed route from the local node to the destination node"
      -d,regexp   "integer.pos.>=0"
      -d,error "-E-argv:bad.path"
      -d,maxvalue "0xff"

      -f,name  "failed.retry"
      -f,default  1
      -f,param "fail-rtry"
      -f,desc  "number of retries of sending a specific packet"

      -i,name  "dev.idx"
      -i,default  ""
      -i,param "dev-index"
      -i,desc  "Specifies the index of the device of the port used to connect to the IB fabric (in case of multiple devices on the local system)"
      -i,regexp   "integer.pos.==1"
      -i,error "-E-argv:not.pos.integer"
      -i,maxvalue "0xff"

      -k,name  "sm.key"
      -k,desc  "in order to generate \"trusted\" queries the user needs to provide the SM_key"
      -k,regexp   "integer:nonneg:==1"
      -k,error "-E-argv:not.nonneg.integer"

      -l,name  "lid.route"
      -l,param "lid"
      -l,desc "The LID of destination port"
      -l,regexp "integer.pos.==1"
      -l,error "-E-argv:not.pos.integer"
      -l,maxvalue "0xffff"

      -m,name  "method"
      -m,param "meth"
      -m,desc  "send the mad using the given method"

      -n,name  "by-name.route"
      -n,param "name"
      -n,desc  "the name of the destination port"
      -n,regexp   {^[^ ,]+$}
      -n,error "-E-argv:bad.node.name"

      -o,name  "out.dir"
      -o,default  "/tmp"
      -o,param "out-dir"
      -o,desc  "Specifies the directory where the output files will be placed"

      -p,name  "port.num"
      -p,default  ""
      -p,param "port-num"
      -p,desc  "Specifies the local device's port number used to connect to the IB fabric"
      -p,regexp   "integer.pos.==1"
      -p,error "-E-argv:not.pos.integer"
      -p,maxvalue "0xff"

      -q,name  "query.mode"
      -q,desc  "starts a \"query mode\""
      -q,arglen   0

      -r,name  "report"
      -r,desc  "Provides a report of the fabric qualities"
      -r,arglen   0

      -s,name  "sys.name"
      -s,default  ""
      -s,param "sys-name"
      -s,desc  "Specifies the local system name. Meaningful only if a topology file is specified"

      -t,name  "topo.file"
      -t,param "topo-file"
      -t,desc  "Specifies the topology file name"

      -v,name  "verbose"
      -v,desc  "Instructs the tool to run in verbose mode"
      -v,arglen   0

      ## Support verbosity with verbose levels
      #   -v,name "verbose"
      #   -v,desc "Instructs the tool to run in verbose mode"
      #   -v,arglen  0..1
      #   -v,regexp  "integer.nonneg.<=1"
      #   -v,error   "-E-argv:not.nonneg.integer"
      #   -v,save     "-v,arglen 0..1"
      #   -v,default  0
      #   -v,maxvalue "0xffff"

      -w,name  "wait.time"
      -w,default  0
      -w,param "wait-ms"
      -w,desc  "the time in msec to wait from receiving the response to the next send"
      -w,regexp   "integer.nonneg.==1"
      -w,error "-E-argv:not.nonneg.integer"
      -w,maxvalue "1000000"

      -h,name  "help"
      -h,desc  "Prints this help information"
      -h,longflag "--help"

      -V,name  "version"
      -V,desc  "Prints the version of the tool"
      -V,longflag "--version"

      --vars,name "version"
      --vars,desc "Prints the tool's environment variables and their values"
   }

   ### Edit InfoArgv with params
   set InfoArgv(-skip,regexp) ^(([join $G(var:list.skip.steps) )|(]))$
   set tmpReg "=(0x)?0*\[a-fA-F0-9\]+"
   set InfoArgv(-P,regexp) "^(([ join $G(var:list.pm.counter) $tmpReg)|( ])$tmpReg)$"

   ### Some changes from the default definitions
   # (e.g., for ibdiagpath, since it recieves addresses of two ports...)
   switch -exact -- $G(var:tool.name) {
      "ibdiagui"  -
      "ibdiagnet" {
         array set InfoArgv {
            -c,desc     "The minimal number of packets to be sent across each link"

            -pm,desc "Dumps all pmCounters values into ibdiagnet.pm"
            -wt,desc "Write out a topology file for the discovered topology"
         }
      }
      "ibdiagpath" {
         array set InfoArgv {
            -d,desc "directed route from the local node (which is the source) and the destination node"

            -pm,desc "Dumps all pmCounters values into ibdiagpath.pm"

            -l,param "\[src-lid,\]dst-lid"
            -l,desc  "Source and destination LIDs (source may be omitted -> local port is assumed to be the source)"
            -l,regexp   "integer.pos.>=1&<=2"
            -l,error "-E-argv:not.pos.integers"

            -n,param "\[src-name,\]dst-name"
            -n,regexp   {^([^ ,]+,)?[^ ,]+$}

            -v,desc  "Provide full verbosity about the checks performed on every port"

            -c,default  100
            -c,desc  "The number of packets to be sent from source to destination nodes"
         }
         set InfoArgv(-n,desc)    "Names of the source and destination ports "
         append InfoArgv(-n,desc) "(as defined in the topology file; source may be omitted "
         append InfoArgv(-n,desc) "-> local port is assumed to be the source)"
      }
      "ibsac" {
         set InfoArgv(-m,desc) "method to be used - either get or gettable"
         set InfoArgv(-a,desc) "the specific attribute to send"
      }
      "ibcfg"     {
         array set InfoArgv {
            -c,name "config.mode"
            -c,default ""
            -c,param "cmd"
            -c,desc  "defines the command to be executed"
            -c,regexp "cmd.name"
            -c,arglen "1.."
            -c,error  "cmd"
         }
      }
      "ibping"    {
         set InfoArgv(-c,desc) "the total number of packets to be sent"
         set InfoArgv(-c,default) 100
      }
      default {
      }
   }
}

proc UpdateInfoArgv_Win {} {
   ### Define here new directory path, when installing for windows
   global tcl_platform env InfoArgv
   if {[info exists tcl_platform(platform)] } {
      switch -exact -- $tcl_platform(platform) {
         "windows" {
            if {[info exists env(Temp)]} {
               set InfoArgv(-o,default) $env(Temp)
            } elseif {[info exists env(Tmp)]} {
               set InfoArgv(-o,default) $env(Tmp)
            } else {
               set InfoArgv(-o,default) "\tmp"
            }
         }
      }
   }
   return 0
}

proc UpdateInfoArgv_Pkg {_flagInfo _flag} {
   global G InfoArgv TOOLS_FLAGS

   set toolNameIndx [lsearch  $_flagInfo "*,tool.name *"]
   if {$toolNameIndx == -1} {
      return -1
   }
   set listOfTools [lrange [lindex $_flagInfo $toolNameIndx] 1 end]
   set flag [string trimleft $_flag -]
   foreach toolName $listOfTools {
      if {![info exists TOOLS_FLAGS($toolName)]} {
         return -1
      }
   }

   set res 1
   foreach listEntry $_flagInfo {
      if {[llength $listEntry] < 2} {
         continue
      }
      set arrayEntry [lindex $listEntry 0]
      if {($arrayEntry == "$_flag,tool.name") || ($arrayEntry == "$_flag,proc.info")} {
         continue
      }
      set arrayInfo [lrange $listEntry 1 end]

      if {[info exists InfoArgv($arrayEntry)]} {
         continue
      }
      set res 0
      set InfoArgv($arrayEntry) $arrayInfo
   }
   return $res
}

proc SetToolsFlags {} {
   # Flags encompassed in ( ) are mandatory.
   # If a few flags are thus encompassed, then they are mutex
   global TOOLS_FLAGS
   array set TOOLS_FLAGS {
      ibping     "(n|l|d) . c w v o     . t s i p "
      ibdiagpath "(n|l|d) . c   v o smp . t s i p    . pm pc P . lw ls sl ."
      ibdiagui   "          c   v r o   . t s i p    . pm pc P . lw ls ."
      ibdiagnet  "          c   v r o   . t s i p wt . pm pc P . lw ls    . skip load_db"
      ibcfg    "(n|l|d) (c|q)       . t s i p o"
      ibmad    "(m) (a) (n|l|d)     . t s i p o ; (q) a"
      ibsac    "(m) (a) k           . t s i p o ; (q) a"

      envVars     "t s i p o load_db"
      general     "h V -vars"
   }
}

proc UpToolsFlags {_flag _tool} {
   global TOOLS_FLAGS
   if {![info exists TOOLS_FLAGS($_tool)]} {
      return 1
   } else {
      append TOOLS_FLAGS($_tool) " $_flag"
      return 0
   }
}

proc GetToolsFlags { tool } {
   global TOOLS_FLAGS
   if {[info exists TOOLS_FLAGS($tool)]} {
      return $TOOLS_FLAGS($tool)   
   }
   return ""
}

proc HighPriortyFlag {} {
   global argv
   ### If help is needed ...
   if { [BoolWordInList "-h" $argv] || [BoolWordInList "--help" $argv] } {
      inform "-H-help"
   }
   ### Output the application version
   if { [BoolWordInList "-V" $argv] || [BoolWordInList "--version" $argv] } {
      inform "-H-version"
   }
   ### List all env-var names
   if { [BoolWordInList "--vars" $argv] } {
      inform "-H-vars"
   }
}

proc SetDefaultValues {} {
   global G argv env InfoArgv
   foreach entry [array names InfoArgv "*,default"] {
      set flag [lindex [split $entry ,] 0]
      if {[catch { set name $InfoArgv($flag,name)}]} { continue; }
      if { $InfoArgv($entry) != "" } {
         set G(argv:$name) $InfoArgv($entry)
      }
   }
}

proc SetEnvValues {} {
   global G argv env InfoArgv
   foreach flag [GetToolsFlags envVars]  {
      set name $InfoArgv(-$flag,name)
      set envVarName "IBDIAG_[string toupper [join [split $name .] _]]"
      if { ! [info exists env($envVarName)] } { continue; }
      if { $env($envVarName) == "" } { continue; }
      set G(argv:$name) $env($envVarName)
   }
}

######################################################################
### parsing the command-line arguments
######################################################################
proc ParseArgv {} {
   global G argv env InfoArgv

   ### For help/version/vars requests
   HighPriortyFlag

   ### Setting default values for command-line arguments
   SetDefaultValues

   ### Define flags values by means of environment variables
   SetEnvValues

   ## Command line check - Test1.0: for the case when there are two different operatinal
   # modes of the tool like the case is for ibmad,ibsac
   set toolsFlags [join [split [GetToolsFlags $G(var:tool.name)] "." ]]

   foreach section [split $toolsFlags ";" ] {
      foreach item $section {
         if { ! [regexp {\(} $item] } { continue; }
         if { [regsub -all " $item " " $argv " {} .] == 0 } {
            catch { unset infoFlags }
            break;
         }
         if {[info exists infoFlags]} {
            if { $infoFlags != $section } {
               set tmpFlags [list [join [lindex $infoFlags 0] [lindex $section 0]]]
               inform "-E-argv:nonmutex.flags" -flags $tmpFlags
            }
         }
         set infoFlags $section
      }
   }

   if { ! [info exists infoFlags] } {
      set infoFlags [lindex [split $toolsFlags ";" ] 0]
   }

   ## Command line check - Test2.0: for duplicated flags, and mandetory option
   foreach item $infoFlags {
      set mandatory [regsub -all {[()]} $item "" item]
      set litem "-[join [split $item |] " -"]"
      set mutexList ""
      foreach arg $argv {
         append mutexList " " [lindex $litem [lsearch -exact $litem $arg]]
      }
      set mutexList [lsort $mutexList]
      for { set i 0 } { $i < [llength $mutexList] } { incr i } {
         if { [lindex $mutexList $i] == [lindex $mutexList [expr $i+1]] } {
            inform "-E-argv:flag.twice.specified" -flag [lindex $mutexList $i]
         }
      }
      if { [llength $mutexList] > 1 } {
         inform "-E-argv:nonmutex.flags" -flags $mutexList
      }
      if { ( [llength $mutexList] == 0 ) && $mandatory } {
         inform "-E-argv:missing.mandatory.flag" -flag [list $litem]
      }
   }

   set argvList $argv

   regsub -all {[()|]} $infoFlags " " allLegalFlags
   set allLegalFlags "-[join $allLegalFlags { -}]"

   while { [llength $argvList] > 0 } {
      set flag  [lindex $argvList 0]
      set value [lindex $argvList 1]

      if { ! [BoolWordInList $flag $allLegalFlags] } {
         inform "-E-argv:unknown.flag" -flag $flag
      }
      set argvList [lreplace $argvList 0 0]
      if {[BoolWordInList $flag $argvList]} {
         inform "-E-argv:flag.twice.specified" -flag $flag
      }

      set arglen 1
      catch { set arglen $InfoArgv($flag,arglen) }
      if { [llength $argvList] == 0 && ![regexp {^0} $arglen] } {
         inform "-E-argv:flag.without.value" -flag $flag
      }

      ### Checking values validity and setting G(argv:$name) - the flag's value
      set regexp 1
      if { [regexp {^0} $arglen] && [regexp {^\-} $value] } {
         set value ""
      } elseif {[info exists InfoArgv($flag,regexp)]} {
         scan [split $InfoArgv($flag,regexp) .] {%s %s %s} regType sign len
         switch $regType {
            "file" {
               scan [split $InfoArgv($flag,regexp) .] {%s %s %s} regType state attributes
               set attributes [split $attributes ,]
               set fn $value
               #set fn [file normalize $value]
               if {$state == "exists"} {
                  if { ! [file isfile $fn] } {
                     inform "-E-argv:file.not.found" -flag $flag -value $fn
                  }
               }
               foreach attribute $attributes {
                  switch $attribute {
                     "readable" -
                     "writable" {
                        if {![file $attribute $fn]} {
                           inform "-E-argv:file.not.$attribute" -flag $flag -value $fn
                        }
                     }
                     "not_readable" -
                     "not_writable" {
                        set attribute [lindex [split $attribute _] end]
                        if {![file $attribute $fn]} {
                           inform "-E-argv:file.not.$attribute" -flag $flag -value $fn
                        }
                     }
                  }
               }
            }
            "integer" {
               ### Turn values like 010 into 10 (instead of the tcl value - 8)
               set int "(0x)?0*(\[a-fA-F0-9\]+)"
               set valuesList [split $value ","]
               ### Checking if length of integers list is OK
               foreach condition [split $len &] {
                  if { ! [expr [llength $valuesList] $condition] } {
                     inform "$InfoArgv($flag,error)" -flag $flag -value $value
                  }
               }
               ### Checking that all items in list are integers
               set formattedValue ""
               if {[catch { set maxValue $InfoArgv($flag,maxvalue) }]} {
                  set maxValue $G(var:maximal.integer)
               }
               if {[catch { set minValue $InfoArgv($flag,minvalue) }]} {
                  set minValue $G(var:minimal.integer)
               }
               foreach item $valuesList {
                  ### Special case: I allow the command -d ""
                  if { ( [llength [join $item]] == 0 ) && ( [llength $valuesList] == 1 ) } {
                     break;
                  }
                  if {[catch {format %x $item} err]} {
                     if {[regexp "integer value too large to represent" $err]} {
                        inform "-E-argv:too.large.integer" -flag $flag -value $value
                     } else {
                        inform "$InfoArgv($flag,error)" -flag $flag -value $value
                     }
                  }
                  if {[string length $item] > [string length $G(var:maximal.integer)]} {
                     inform "-E-argv:too.large.integer" -flag $flag -value $value
                  }
                  if { ! [regsub "^$int$" $item {\1\2} item] } {
                     inform "$InfoArgv($flag,error)" -flag $flag -value $value
                  }
                  if { $sign == "pos" && $item == 0 } {
                     inform "$InfoArgv($flag,error)" -flag $flag -value $value
                  }
                  if {$item > $maxValue} {
                     inform "-E-argv:too.large.value" -flag $flag -value $value -maxValue $maxValue
                  }
                  if {$item < $minValue} {
                     inform "-E-argv:too.small.value" -flag $flag -value $value -minValue $minValue
                  }
                  lappend formattedValue [format %d $item]
               }
               set value [join $formattedValue ,]
            }
            default {
               set regexp [regexp $InfoArgv($flag,regexp) "$value"]
               if {!$regexp} {
                  inform "$InfoArgv($flag,error)" -flag $flag -value $value
               }
            }
         }
      }
      set name $InfoArgv($flag,name)
      # DZ: bug remove it :set G(argv:$name) ""
      if { ($arglen == "1..") || ($arglen > 1) } {
         if { [set next_flagIndx [lsearch -regexp $argvList {^\-}]] == -1 } {
            set next_flagIndx [llength $argvList]
         }
         append G(argv:$name) " " [lindex $argvList 0]
         set argvList [lreplace $argvList 0 0]
         if {$next_flagIndx > 1} {
            set argvList [concat $flag $argvList]
         }
      } elseif { $arglen == "0" || ( ( $arglen == "0..1" ) && ( ( $regexp==0 ) || ( $value=="" ) ) ) } {
         set G(argv:$name) 1
      } elseif { $regexp } {
         set G(argv:$name) "$value"
         set argvList [lreplace $argvList 0 0]
      } else {
         inform "$InfoArgv($flag,error)" -flag $flag -value $value
      }
   }

   ## Command line check - Test3.0: If we are using direct-route addressing,
   # the output port of the direct route must not disagree with the
   # local port number (if specified).
   # If the latter was not specified, it is set to be the former.
   # Need to check for -i flag also, however those two still need to match
   if {[info exists G(argv:direct.route)]} {
      set drOutPort [lindex [split $G(argv:direct.route) ","] 0]
      if { $drOutPort == "" } {
         # do nothing
      } elseif {[info exists G(argv:port.num)]} {
         if { $drOutPort != $G(argv:port.num) } {
            inform "-E-argv:disagrees.with.dir.route" -value $G(argv:port.num) -port $drOutPort
         }
      } else {
         set G(argv:port.num) $drOutPort
         set G(-p.set.by.-d) 1
      }
   }

   ## Command line check - Test4.0: directories and files
   if {[info exists G(argv:out.dir)]} {
      set dir $G(argv:out.dir)
      ### Special deal for windows
      global tcl_platform
      if {[info exists tcl_platform(platform)] } {
         switch -exact -- $tcl_platform(platform) {
            "windows" {
               regsub -all \"  $dir "" dir
            }
         }
      }
      if { ! [file isdirectory $dir] } {
         if {[catch {file mkdir $dir} msg] } {
            inform "-E-argv:could.not.create.dir" -flag "-o" -value $dir -errMsg [list $msg]
         }
      } elseif { ! [file writable $dir] } {
         inform "-E-argv:dir.not.writable" -flag "-o" -value $dir
      }
      foreach extention $G(var:list.files.extention) {
         set G(outfiles,.${extention}) [file join $dir $G(var:tool.name).${extention}]
      }
   }

   ## Command line check - Test5.0: log file
   if {[catch {set G(logFileID) [open $G(outfiles,.log) w]} errMsg]} {
      inform "-E-loading:cannot.open.file" $G(outfiles,.log) -fn $G(outfiles,.log) -errMsg $errMsg
   }

   ## Command line check - Test6.0: If topology is not given and -s/-n  flags are specified
   if { ! [info exists G(argv:topo.file)]  } {
      if {[info exists G(argv:sys.name)]} {
         inform "-W-argv:-s.without.-t"
      }
      if {[info exists G(argv:by-name.route)]} {
         inform "-E-argv:nodename.without.topology" -value $G(argv:by-name.route)
      }
      inform "-W-argv:no.topology.file"
      ## Command line check - Test7.0: If topology is given, check that $G(argv:by-name.route)
      # are names of existing nodes we do the same for G(argv:sys.name),
      # after ibis_init - to search for sys.name in the description of the source node.
   } else {
      set topoFile $G(argv:topo.file)
      if { ! [file isfile $topoFile] } {
         inform "-E-argv:file.not.found" -flag "-t" -value $topoFile
      } elseif { ! [file readable $topoFile] } {
         inform "-E-argv:file.not.readable" -flag "-t" -value $topoFile
      }
   }

   return
}
##############################

######################################################################
### User messages
######################################################################

proc SummaryMessage {} {
   global G
   if {$G(status:summary.headr) != 0} {
      set prevHeadr $G(status:summary.headr)
      set warns $G(status:summary.warn)
      set errs  $G(status:summary.errs)
      append G(status:summary.text) \
         [format "    %-40s %-6d %-6d\n" $prevHeadr $errs $warns]
      set G(status:summary.errs) 0
      set G(status:summary.warn) 0
   }
   puts "----------------------------------------------------------------"
   puts "-I- Stages Status Report:"
   puts [format "    %-40s %-6s %-6s" STAGE Errors Warnings]
   puts $G(status:summary.text)
   puts "Please see $G(outfiles,.log) for complete log"
   puts "----------------------------------------------------------------"
}

##############################
proc PutsIn80Chars { string args } {
   global G
   set maxLen 80
   set indent ""
   if { [llength $args] == 1 } { set args [join $args] }
   set chars       [GetWordAfterFlag $args "-chars"]
   set nonewline   [BoolWordInList "-nonewline" $args]
   if {[BoolWordInList "-length" $args]} {
      set maxLen [GetWordAfterFlag $args "-length"]
   }
   foreach line [split $string \n] {
      if { $chars != "" } {
         if { [set idx [string first $chars $line]] >= 0 } {
            set indent "[string range $line 0 [expr $idx -1]]$chars"
            set line [string range $line [expr $idx+[string length $chars]] end]
         }
      } else {
         regexp {^(-[A-Z]- ?| +)(.*)$} $line . indent line
      }
      set outline "$indent"
      set indent [Bar " " [string length $indent]]
      set len80 [expr $maxLen - [string length $indent]]
      while { [string length $line] > 0 } {
         set interval [string range $line 0 $len80]
         if { [BoolWordInList "-origlen" $args] \
                 || ( [string length $line] <= $len80 ) \
                 || ( [set spcIndx [string last " " $interval]] == -1 ) } {
            append outline "$line"
            break;
         } else {
            append outline "[string range $line 0 [expr $spcIndx -1]]\n$indent"
            set line [string range $line [expr $spcIndx +1] end]
         }
      }
      if { ![BoolWordInList "-NoStdout" $args] } {
         if {$nonewline} {
            puts -nonewline $outline
         } else {
            puts $outline
         }
      }
      if { ![BoolWordInList "-NoLogFile" $args] } {
         if {$nonewline} {
            catch { puts -nonewline $G(logFileID) $outline }
         } else {
            catch { puts $G(logFileID) $outline }
         }
      }
   }
}
##############################

##############################
proc retriveEntryFromArray {_arrayName _entry {_defMsg "UNKNOWN"}} {
   upvar 1 $_arrayName tmpArray
   if {[info exists tmpArray($_entry)]} {
      set res $tmpArray($_entry)
      return $res
   } else {
      return $_defMsg
   }
}
##############################

##############################
proc inform { msgCode args } {
   global G InfoArgv argv env ibdiagSourceVersion
   regexp {^(\-[A-Z]\-)?([^:]+).*$} $msgCode . msgType msgSource

   if { $msgType == "-V-" } {
      if { ![info exists G(argv:verbose)] } {
         return
      }
      set dontShowMads \
         [expr ( \"[ProcName 1]\" == \"DiscoverFabric\" ) \
             && ( \"$G(var:tool.name)\" != \"ibdiagnet\" ) \
             && ( ![info exists G(argv:verbose)] ) ]
   }

   if {! [info exist G(status:high.priorty)]} {
      ### Setting Error Codes
      set G(status:high.priorty)       0
      set G(status:discovery.failed)   1
      set G(status:illegal.flag.value) 2
      set G(status:ibis.init)          3
      set G(status:root.port.get)      4
      set G(status:topology.failed)    5
      set G(status:loading)            6
      set G(status:crash)              7
      # Summary generation
      set G(status:summary.text) ""
      set G(status:summary.errs) 0
      set G(status:summary.warn) 0
      set G(status:summary.headr) 0
   }

   ##################################################
   ### When general tool's info is requested (help page, version num etc.)
   if { $msgType == "-H-" } {
      switch -exact -- $msgCode {
         "-H-help" {
            showHelpPage
         } "-H-version" {
            append msgText "-I- $G(var:tool.name) version $G(var:version.num) source $ibdiagSourceVersion"
         } "-H-vars" {
            append msgText "-I- $G(var:tool.name) environment variabls:"
            foreach flag [GetToolsFlags envVars]  {
               set name $InfoArgv(-$flag,name)
               set envVarName  IBDIAG_[string toupper [join [split $name .] _]]
               append msgText "\n    $envVarName \t"
               if {[catch {append msgText "$env($envVarName)"}]} {
                  append msgText "not set"
               }
            }
         }
      }
      catch { PutsIn80Chars $msgText }
      exit $G(status:high.priorty)
   }
   set argsList $args

   array set msgF {}
   while { [llength $argsList] > 0 } {
      if {[regsub "^-" [lindex $argsList 0] "" param]} {
         set value [lindex $argsList 1]
         ### Not [join $value] - because of -I-ibdiagnet:bad.link
         set msgF($param) $value ;
         set argsList  [lreplace $argsList 0 1]
      } else {
         set argsList  [lreplace $argsList 0 0]
      }
   }
   set listOfNames ""
   set listOfNames_Ports ""
   set listOfNames_EntryPorts ""

   set maxType 6
   set total 0
   set localDevice 0
   array set deviceNames { SW "Switch" CA "HCA" Rt "Router" }
   foreach entry [lsort [array names msgF DirectPath*]] {
      set i $total
      incr total
      if {[catch {set NODE($i,Type) [GetParamValue Type $msgF($entry)]}]} {
         set NODE($i,Type) ""
         set NODE($i,FullType) ""
      } else {
         set NODE($i,FullType) [GetDeviceFullType $NODE($i,Type)]
      }
      if {[catch {set NODE($i,PortGUID) [GetParamValue PortGUID $msgF($entry)]}]} {
         set NODE($i,PortGUID) ""
      }
      set PATH($i) [ArrangeDR $msgF($entry)]
      set NODE($i,EntryPort) [GetEntryPort $msgF($entry)]
      if {[info exists msgF(port${i})]} {
         set NODE($i,EntryPort) $msgF(port${i})
      }

      set DrPath2Name_1 [DrPath2Name $msgF($entry) -fullName]
      set DrPath2Name_2 [DrPath2Name $msgF($entry)]

      if {$NODE($i,Type) == "SW"} {
         set DrPath2Name_3 [DrPath2Name $msgF($entry) -port 0]
      } else {
         set DrPath2Name_3 [DrPath2Name $msgF($entry) -port $NODE($i,EntryPort)]
      }

      set DrPath2Name_4 [DrPath2Name $msgF($entry) -port $NODE($i,EntryPort)]
      set DrPath2Name_5 [DrPath2Name $msgF($entry) -nameLast -fullName -port $NODE($i,EntryPort)]
      set DrPath2Name_6 [DrPath2Name $msgF($entry) -nameLast -fullName]

      if { $msgF($entry) == "" } {
         set NODE($i,FullName)       "$G(var:desc.local.dev) $DrPath2Name_1"
         set NODE($i,Name)           "$G(var:desc.local.dev) \"$DrPath2Name_2\""
         set NODE($i,Name_Port)      "$G(var:desc.local.dev) \"$DrPath2Name_3\""
         set NODE($i,Name_EntryPort) "$G(var:desc.local.dev) \"$DrPath2Name_4\""
         set NODE($i,FullNamePort_Last)  "$DrPath2Name_5"
         set NODE($i,FullName_Last)  "$DrPath2Name_6"
         set localDevice 1
      } else {
         set NODE($i,FullName)       "$DrPath2Name_1"
         set NODE($i,Name)           "\"$DrPath2Name_2\""
         set NODE($i,Name_Port)      "\"$DrPath2Name_3\""
         set NODE($i,Name_EntryPort) "\"$DrPath2Name_4\""
         set NODE($i,FullNamePort_Last)  "$DrPath2Name_5"
         set NODE($i,FullName_Last)  "$DrPath2Name_6"
      }

      if {$DrPath2Name_2 == ""} {
         set NODE($i,Name) ""
         if {$NODE($i,Type) != "SW"} {
            set NODE($i,Name_Port) "$DrPath2Name_3"
         } else {
            set NODE($i,Name_Port) ""
         }
      }

      lappend listOfNames $NODE($i,Name)
      lappend listOfNames_Ports $NODE($i,Name_Port)
      lappend listOfNames_EntryPorts $NODE($i,Name_EntryPort)
   }
   set maxName [GetLengthMaxWord $listOfNames]
   if {[info exists msgF(maxName)]} {
      set maxName  $msgF(maxName)
   }
   set maxName_Port [GetLengthMaxWord $listOfNames_Ports]
   if {[info exists msgF(maxName_Port)]} {
      set maxName_Port  $msgF(maxName_Port)
   }
   set maxName_EntryPort [GetLengthMaxWord $listOfNames_EntryPorts]

   for {set i 0} {$i < $total } {incr i} {
      set NODE($i,FullType,Spaces)        [AddSpaces $NODE($i,FullType) $maxType]
      set NODE($i,Name,Spaces)            [AddSpaces $NODE($i,Name) $maxName]
      set NODE($i,Name_Port,Spaces)       [AddSpaces $NODE($i,Name_Port) $maxName_Port]
      set NODE($i,Name_EntryPort,Spaces)  [AddSpaces $NODE($i,Name_EntryPort) $maxName_EntryPort]
   }
   if {[info exists msgF(flag)]} {
      set name  ""
      catch { set name $InfoArgv($msgF(flag),name) }
      set envVarName "IBDIAG_[string toupper [join [split $name .] _]]"
      if {[BoolWordInList "$msgF(flag)" $argv]} {
         set llegalValMsg "llegal value for $msgF(flag) option"
      } elseif {[info exists env($envVarName)]} {
         set llegalValMsg "llegal value for environment variable $envVarName"
      } elseif { ( $msgCode == "-E-localPort:port.not.found" ) && \
                    [info exists G(argv:direct.route)] } {
         set msgCode "-E-localPort:illegal.dr.path.out.port"
      } elseif { ( $msgCode == "-E-localPort:port.not.found.in.device" ) && \
                    [info exists G(argv:direct.route)] } {
         set msgCode "-E-localPort:illegal.dr.path.out.port"
      } else {
         set llegalValMsg ""
      }
   }
   set validNames ""
   if {[info exists msgF(names)]} {
      set validNames "[lsort -dictionary $msgF(names)]"
      if { [llength $validNames] > 0 } {
         set validNames "Valid %s names are:\n${validNames}"
      }
   }
   if {[info exists G(argv:failed.retry)] && [info exists G(var:badpath.maxnErrors)]} {
      set numOfRetries [expr $G(argv:failed.retry) + $G(var:badpath.maxnErrors)]
   } else {
      set numOfRetries "DannyZarko"
   }
   set rumSMmsg "To use lid-routing, an SM should be ran on the fabric."
   set bar "${msgType}[Bar - 50]"
   set putsFlags ""
   set msgText "$msgType "
   set headerText ""

   ### Decoding msgCode
   switch -exact -- $msgCode {
      "-E-argv:unknown.flag" {
         append msgText "Illegal argument: Unknown option $msgF(flag)."
      }
      "-E-argv:missing.mandatory.flag" {
         if { [llength [join $msgF(flag)]] > 1 } {
            append msgText "Illegal argument: Missing one of the mandatory options: [join [join $msgF(flag)] ,]."
         } else {
            append msgText "Illegal argument: Missing a mandatory option $msgF(flag)."
         }
      }
      "-E-argv:flag.without.value" {
         append msgText "Illegal argument: Option $msgF(flag) requires an argument."
      }
      "-E-argv:flag.twice.specified" {
         append msgText "Illegal argument: Option $msgF(flag) is specified twice."
      }
      "-E-argv:nonmutex.flags" {
         append msgText "Illegal argument: Options are mutually exclusive [join [join $msgF(flags)] ,]."
      }
      "-E-argv:nonmutex.modes" {
         # TODO: do I ever use this ???
         append msgText "Bad arguments; could not figure out the run mode."
      }
      "-E-argv:too.large.integer" {
         append msgText "Illegal argument: I${llegalValMsg}: $msgF(value)%n"
         append msgText "Integer value too large to represent."
      }
      "-E-argv:not.nonneg.integer" {
         append msgText "Illegal argument: I${llegalValMsg}: $msgF(value)%n"
         append msgText "(Legal value: a non negative integer number)."
      }
      "-E-argv:not.pos.integer" {
         append msgText "Illegal argument: I${llegalValMsg}: $msgF(value)%n"
         append msgText "(Legal value: a positive integer number)."
      }
      "-E-argv:not.pos.integers" {
         append msgText "Illegal argument: I${llegalValMsg}: $msgF(value)%n"
         append msgText "(Legal value: one or two positive integer numbers, separated by a comma)."
      }
      "-E-argv:too.large.value" {
         append msgText "Illegal argument: I${llegalValMsg}: $msgF(value)%n"
         switch -exact -- $msgF(flag) {
            "-d"    { append msgText "(Legal value: maximal legal port number is [expr $msgF(maxValue)])." }
            "-l"    { append msgText "(Legal value: maximal legal LID is $msgF(maxValue))." }
            default { append msgText "(Legal value: maximal legal value is $msgF(maxValue))." }
         }
      }
      "-E-argv:too.small.value" {
         append msgText "Illegal argument: I${llegalValMsg}: $msgF(value)%n"
         switch -exact -- $msgF(flag) {
            default { append msgText "(Legal value: minimal legal value is $msgF(minValue))." }
         }
      }
      "-E-argv:bad.path" {
         append msgText "Illegal argument: I${llegalValMsg}: \"$msgF(value)\"%n"
         append msgText "(Legal value: a direct path, positive integers separated by commas)."
      }
      "-E-argv:dir.not.found" {
         # TODO: do I ever use this ???
         append msgText "$msgF(value) - no such directory.%n"
         append msgText "I${llegalValMsg} (must be an existing directory)."
      }
      "-E-argv:could.not.create.dir" {
         append msgText "Illegal argument: I${llegalValMsg}: $msgF(value)%n"
         append msgText "Failed to create directory: $msgF(value) (for output files).%n"
         append msgText "Error message:%n\"$msgF(errMsg)\""
      }
      "-E-argv:file.not.found" {
         append msgText "Illegal argument: I${llegalValMsg}: $msgF(value)%n"
         append msgText "No such file."
      }
      "-E-argv:dir.not.writable" {
         append msgText "Illegal argument: I${llegalValMsg}: $msgF(value)%n"
         append msgText "Directory is write protected."
         if { $llegalValMsg == "" } {
            append msgText "\n(Use the -o option to use a different directory"
            append msgText " for the output files)"
         }
      }
      "-E-argv:file.not.readable" {
         append msgText "Illegal argument: I${llegalValMsg}: $msgF(value)%n"
         append msgText "File is read protected."
      }
      "-E-argv:file.not.writable" {
         append msgText "Illegal argument: I${llegalValMsg}: $msgF(value)%n"
         append msgText "File is write protected."
      }
      "-E-argv:bad.sys.name" {
         append msgText "Illegal argument: I${llegalValMsg}: $msgF(value)%n"
         append msgText "No such system in the specified topology file : $G(argv:topo.file)%n"
         if {$validNames == ""} {
            append msgText "The topology file : \"$G(argv:topo.file)\" may be currupted."
         } else {
            append msgText "[format $validNames system]"
         }
      }
      "-E-argv:unknown.sys.name" {
         append msgText "Illegal argument: Local system name was not specified.%n"
         append msgText "It must be specified when using a topology file.%n"
         if {$validNames == ""} {
            append msgText "the topology file : \"$G(argv:topo.file)\" may be currupted."
         } else {
            append msgText "[format $validNames system]"
         }
      }
      "-E-argv:bad.node.name" {
         append msgText "Illegal argument: I${llegalValMsg}: $msgF(value)%n"
         append msgText "(Lagel value: one or two Nodes names separated by a comma)."
         if {[format $validNames node] != ""} {
            append msgText "%n[format $validNames node]"
         }
      }
      "-E-argv:nodename.without.topology" {
         append msgText "Illegal argument: If node(s) are specified by name :\"$msgF(value)\""
         append msgText " then a topology file must be given."
      }
      "-E-argv:disagrees.with.dir.route" {
         append msgText "Illegal argument: Conflicting route source ports ($msgF(port) != $msgF(value))%n"
         append msgText "MADs may be sent only through the local port."
      }
      "-E-argv:not.legal.link.width" {
         append msgText "Illegal argument: I${llegalValMsg}: $msgF(value)%n"
         append msgText "(Legal value: 1x | 4x | 12x)."
      }
      "-E-argv:not.legal.link.speed" {
         append msgText "Illegal argument: I${llegalValMsg}: $msgF(value)%n"
         append msgText "(Legal value: 2.5 | 5 | 10)."
      }
      "-E-argv:not.legal.PM" {
         set pmCounterList "\t[join $G(var:list.pm.counter) \n\t]"

         append msgText "Illegal argument: I${llegalValMsg}: $msgF(value)%n"
         if {[info exists msgF(duplicatePM)]} {
            append msgText "PM: \"$msgF(duplicatePM)\" is specified twice.%n"
            append msgText "(Legal value: one or more \"<PM counter>=<Trash Limit>\")."
         } else {
            append msgText "(Legal value: one or more \"<PM counter>=<Trash Limit>\").%n"
            append msgText "Legal PM Counter names are: %n$pmCounterList."
         }
      }
      "-E-argv:not.legal.skip" {
         append msgText "Illegal argument: I${llegalValMsg}: $msgF(value)%n"
         append msgText "(Legal value: dup_guids | zero_guids | pm | logical_state | part | ipoib | all)."
      }

      "-W-argv:-s.without.-t" {
         append msgText "Local system name is specified, but topology "
         append msgText "is not given. The former is ignored."
      }
      "-W-argv:no.topology.file" {
         append msgText "Topology file is not specified.%n"
         append msgText "Reports regarding cluster links will use direct routes."
      }
      "-E-argv:specified.port.not.connected" {
         append msgText "Topology parsing: Invalid value for $msgF(flag) : $msgF(value)%n"
         append msgText "The specified port is not connected to The IBFabric.%n"
         append msgText "(described in : $G(argv:topo.file))"
      }
      "-E-argv:hca.no.port.is.connected" {
         append msgText "Topology parsing: Invalid value for $msgF(flag) : $msgF(value)%n"
         append msgText "None of the ports of $msgF(type) : \"$msgF(value)\" "
         append msgText "is connected to The IBFabric.%n(described in : $G(argv:topo.file))"
      }
      "-W-argv:hca.many.ports.connected" {
         append msgText "Topology parsing: A few ports of $msgF(type) : \"$msgF(value)\" "
         append msgText "are connected to the IBFabric (described in : $G(argv:topo.file))%n"
         append msgText "Port number : $msgF(port) will be used."
      }
      "-E-argv:bad.port.name" {
         append msgText "$msgF(value) - no such port. I${llegalValMsg}.\n"
         append msgText "[format $validNames port]"
      }
      "-E-argv:no.such.command" {
         append msgText "\"$msgF(command)\" - no such command."
      }
      "-E-argv:command.not.valid.for.device" {
         append msgText "Command \"$msgF(command)\" is not valid for device $msgF(device)."
      }
      "-E-argv:no.such.field" {
         append msgText "\"$msgF(errorCode)\" - illegal field for command \"$msgF(command)\"."
      }
      "-E-argv:illegal.field.value" {
         append msgText "\"$msgF(value)\" - illegal value for field "
         append msgText "\"$msgF(field)\" (must be $msgF(legal))."
      }
      "-E-argv:illegal.integer.value" {
         append msgText "\"$msgF(value)\" - illegal value for field "
         append msgText "\"$msgF(field)\" (must be an integer)."
      }
      "-E-argv:illegal.port.value" {
         append msgText "$msgF(value) - illegal port value "
         append msgText "(must be an integer 1..$msgF(ports))."
      }
      "-E-argv:missing.fields" {
         append msgText "The field(s) -[join $msgF(field) ,-] "
         append msgText "must be specified for the command $msgF(command)."
      }
      "-E-argv:fail.to.parse.flag" {
         append msgText "Illegal argument: I${llegalValMsg}: $msgF(cmdLine)%n"
         append msgText "Flag is provided by external package"
      }



      "-E-ibis:ibis_init.failed" {
         append msgText "IBIS: Error from ibis_init: \"$msgF(errMsg)\""
      }
      "-E-ibis:ibis_get_local_ports_info.failed" {
         append msgText "IBIS: Error from ibis_get_local_ports_info: \"$msgF(errMsg)\""
      }
      "-E-ibis:no.hca" {
         append msgText "IBIS: No HCA was found on local machine."
      }
      "-E-ibis:could.not.create.directory" {
         append msgText "IBIS: Failed to create directory (for ibis log file): $msgF(value).%n"
         append msgText "Error message:%n\"$msgF(errMsg)\""
      }
      "-E-ibis:directory.not.writable" {
         append msgText "IBIS: The following directory (for ibis log file) is write protected: $msgF(value)."
      }
      "-E-ibis:file.not.writable" {
         append msgText "IBIS: The following file is write protected: $msgF(value)%n"
         append msgText "Error message: \"$msgF(errMsg)\""
      }
      "-V-ibis:ibis_get_local_ports_info" {
         append msgText "IBIS: ibis_get_local_ports_info:%n$msgF(value)"
      }
      "-V-ibis.ibis.log.file" {
         append msgText "IBIS: ibis log file: $msgF(value)"
      }


      "-W-loading:cannot.load.package" {
         append msgText "Internal Package Error: $msgF(package)%n"
         append msgText "$msgF(error)"
      }
      "-I-loading:load.package" {
         set msgText "Loading $msgF(package)"
      }
      "-E-loading:cannot.use.current.tcl.package" {
         append msgText "The current Tcl version is: Tcl$msgF(version). "
         append msgText "$G(var:tool.name) requires Tcl8.4 or newer."
      }
      "-E-parse.topology" {
         append msgText "Failed to parse topology file $G(argv:topo.file)"
      }
      "-E-loading:cannot.load.package.ibdm" {
         append msgText "Package Loading: Could not load the following package : ibdm.%n"
         append msgText "Error message: \"$msgF(errMsg)\""
      }
      "-W-loading:cannot.load.package.ibdm" {
         append msgText "Package Loading: Could not load the following package : ibdm.%n"
         append msgText "Some options and checks will not be executed"
      }
      "-E-loading:cannot.use.current.ibdm.package" {
         append msgText "Package Loading: The current IBDM version is: IBDM$msgF(version).%n"
         append msgText "$G(var:tool.name) requires IBDM1.1 or newer."
      }
      "-E-loading:cannot.open.file" {
         append msgText "The following file is write protected: $msgF(fn)%n"
         append msgText "Error message: \"$msgF(errMsg)\""
      }
      "-W-loading:old.osm.version" {
         append msgText "OSM: The current OSM version is not up-to-date"
      }
      "-W-loading:old.ibis.version" {
         append msgText "IBIS: The current IBIS version is not up-to-date"
      }
      "-E-loading:old.ibdiag.db" {
          append msgText "Failed to load ibdiag external DB from: $msgF(fn)%n"
          append msgText "Error message: \"$msgF(errMsg)\""
      }
      "-W-loading:external.ibdiag.db" {
          append msgText "Loading external ibdiag DB from: $msgF(fn)"
      }


      "-E-localPort:all.ports.down" {
         if { $G(var:tool.name) == "ibdiagpath" } {
            append msgText "None of the local device ports is in ACTIVE state."
         } else {
            append msgText "All the local device ports are in DOWN state."
         }
      }
      "-E-localPort:all.ports.down.mulitple.devices" {
         if { $G(var:tool.name) == "ibdiagpath" } {
            append msgText "None of the local devices ports is in ACTIVE state."
         } else {
            append msgText "All the local devices ports are in DOWN state."
         }
      }
      "-E-localPort:all.ports.of.device.down" {
         if { $G(var:tool.name) == "ibdiagpath" } {
            append msgText "None of device: $msgF(device) ports is in ACTIVE state."
         } else {
            append msgText "All of device: $msgF(device) ports are in DOWN state."
         }
      }
      "-E-localPort:dev.not.found" {
         append msgText "Local host does not have device number $msgF(value).%n"
         if {$msgF(maxDevices) == 1} {
            append msgText "Only device number: $msgF(maxDevices) is available"
         } else {
            append msgText "Only devices: 1-$msgF(maxDevices) are available"
         }
      }
      "-E-localPort:port.not.found" {
         append msgText "Local device does not have port number: $value."
      }
      "-E-localPort:port.not.found.in.device" {
         append msgText "Device number: $msgF(device) does not have port number: $msgF(port)."
      }
      "-E-localPort:local.port.down" {
         append msgText "Local device port number: $msgF(port) is in DOWN state."
      }
      "-E-localPort:local.port.of.device.down" {
         append msgText "Device $msgF(device) port number: $msgF(port) is in DOWN state."
      }
      "-E-localPort:local.port.not.active" {
         append msgText "Local link (port $msgF(port) of local device) is "
         append msgText "in $msgF(state) state.\n"
         append msgText "(PortCounters may be queried only over ACTIVE links)."
      }
      "-E-localPort:local.port.of.device.not.active" {
         append msgText "Local link (port $msgF(port) of device $msgF(device)) is "
         append msgText "in $msgF(state) state.\n"
         append msgText "(PortCounters may be queried only over ACTIVE links)."
      }
      "-W-localPort:few.ports.up" {
         append msgText "A few ports of local device are up.\n"
         append msgText "Since port-num was not specified (-p option), "
         #append msgText "port $G(argv:port.num) of device $G(argv:dev.idx) "
         append msgText "port $msgF(port) of device $msgF(device) "
         append msgText "will be used as the local port."
      }
      "-W-localPort:few.devices.up" {
         append msgText "A few devices on the local machine have an active port $msgF(port).\n"
         #append msgText "A few devices on the local machine have an active port $G(argv:port.num).\n"
         append msgText "Since device-index was not specified (-i option), "
         #append msgText "port $G(argv:port.num) of device $G(argv:dev.idx) "
         append msgText "port $msgF(port) of device $msgF(device) "
         append msgText "will be used as the local port."
      }

      "-E-localPort:illegal.dr.path.out.port" {
         append msgText "Illegal value for -d option: "
         append msgText "Local device does not have port number $value.%n"
         append msgText "No such direct route."
      }
      "-I-localPort:one.port.up" {
         append msgText "Using port $msgF(port) as the local port."
         #append msgText "Using port $G(argv:port.num) as the local port."
      }
      "-I-localPort:is.dr.path.out.port" {
         append msgText "Using port $msgF(port) as the local port%n"
         #append msgText "Using port $G(argv:port.num) as the local port%n"
         append msgText "(since that is the output port based on the provided direct route)."
      }
      "-I-localPort:using.dev.index" {
         append msgText "Using device $msgF(device) as the local device."
         #append msgText "Using device $G(argv:dev.idx) as the local device."
      }
      "-W-localPort:node.intelligently.guessed" {
         append msgText "Local system name was not specified (-s option).%n"
         append msgText "\"$G(argv:sys.name)\" will be used as the local system name."
      }
      "-E-localPort:local.port.crashed" {
         append msgText "Discovery at local link failed: "
         if {![catch {set portState [GetParamValue LOG $msgF(DirectPath0) -port $NODE(0,EntryPort) -byDr]}]} {
            if {$portState == "DWN"} {
               append msgText "[DrPath2Name "" -port $NODE(0,EntryPort)] is DOWN%n"
            }
         }
         append msgText "$msgF(command) - failed $numOfRetries consecutive times."
      }
      "-E-localPort:local.port.failed" {
         append msgText "Local link is bad: $msgF(command) - failed $msgF(fails) "
         append msgText "times during $msgF(attempts) attempts."
      }
      "-E-localPort:port.guid.zero" {
         append msgText "Unable to use PortGUID = $G(data:root.port.guid) as the local port."
      }
      "-E-localPort:enable.ibis.set.port" {
         append msgText "Failed running : \"ibis_set_port $G(data:root.port.guid)\""
      }   

      "-W-outfile:not.writable" {
         append msgText "Output file $msgF(file0) is write protected.\n"
         append msgText "Writing info into $msgF(file1)."
      }
      "-E-outfile:not.valid" {
         append msgText "Output file $msgF(file0) is illegal value for $G(var:tool.name).\n"
      }


      "-E-discover:broken.func" {
         append msgText "Could not complete discovering the fabric.%n"
         append msgText "Reports will includes only the discovered part.%n"
         set noExiting 1
      }
      "-E-discover:duplicated.guids" {
         if {$localDevice} {set G(LocalDeviceDuplicated) 1}
         append msgText "Duplicate $msgF(port_or_node) guid detected.\n"
         append msgText "The following two different devices:\n"
         append msgText "a $NODE(0,Type,Spaces) $NODE(0,Name_Port,Spaces) $NODE(0,PortGUID) direct path=\"$PATH(0)\"\n"
         append msgText "a $NODE(1,Type,Spaces) $NODE(1,Name_Port,Spaces) $NODE(1,PortGUID) direct path=\"$PATH(1)\"\n"
         append msgText "have inedtical $msgF(port_or_node) guid $msgF(guid)."
         set noExiting 1
      }
      "-E-discover:zero/duplicate.IDs.found" {
         if {($msgF(ID) == "SystemGUID") && ($msgF(value) == 0)} {
            set msgText "-W- "
         }
         append msgText "Found "
         set dontTrimLine 1
         if {$localDevice} {set G(LocalDeviceDuplicated) 1}
         if {$total > 1} {
            append msgText "$total Devices with "
         } else {
            append msgText "Device with "
         }
         if { $msgF(value) != 0 } { append msgText "identical "}
         append msgText "$msgF(ID)=$msgF(value):"

         for {set i 0} {$i < $total} {incr i} {
            append msgText "%n"
            append msgText "a $NODE($i,FullType,Spaces) $NODE($i,Name_Port,Spaces)"
            if {$msgF(ID) != "PortGUID"} {
               append msgText " PortGUID=$NODE($i,PortGUID)"
            }
            append msgText " at direct path=\"$PATH($i)\""
            if {[BoolIsMaked $NODE($i,PortGUID)]} {
               if {$msgF(ID) != "PortGUID"} {
                  append msgText " (duplicate portGUID)"
               } else {
                  append msgText " (masked to PortGUID=$NODE($i,PortGUID))"
               }
            }
         }
         #append msgText %n
         set noExiting 1
      }
      "-I-discover:discovery.status" {
         append putsFlags " -nonewline"
         if {![info exists msgF(log)]} {
            append putsFlags " -NoLogFile"
         }
         if { [info exists G(argv:verbose)] } { return }
         set nodesNum [expr $G(data:counter.SW) + $G(data:counter.CA)]
         append msgText "Discovering ... $nodesNum nodes "
         append msgText "($G(data:counter.SW) Switches & $G(data:counter.CA) CA-s) "
         append msgText "discovered.\r"

      }
      "-V-discover:discovery.status" {
         append msgText "Discovering DirectPath (no. $msgF(index)) \{[ArrangeDR $msgF(path)]\}"
      }

      "-I-reporting:found.roots" {
         set roots [lindex $args 0]
         append msgText "Found [llength $roots] Roots:\n"
         foreach r $roots {
            append msgText "[IBNode_name_get $r]\n"
         }
      }
      "-I-reporting:skip.set.no.report" {
         append msgText "skip option set. no report will be issued"
      }


      "-E-topology:bad.path.in.name.tracing" {
         append msgText "Direct Path \"[ArrangeDR $msgF(path)]\" to \"$msgF(name)\" is bad.%n"
         append msgText "Try running ibdiagpath again byDr with the provided route."
      }
      "-E-topology:bad.sysName.or.bad.topoFile" {
         append msgText "Unable to retrive a route from the local host to \"$msgF(name)\".%n"
         append msgText "Either the given topology file is bad "
         append msgText "or the local sys name is incorrect."
      }
      "-E-topology:no.route.to.host.in.topo.file" {
         append msgText "Unable to retrive a route from the local host to \"$msgF(name)\".%n"
         append msgText "based on the topology given in:$msgF(topo.file)"
      }
      "-E-topology:lid.by.name.failed" {
         append msgText "Couldn't retrive $msgF(name) LID%n"
         append msgText "Please check your topology file and given sys name."
      }
      "-E-topology:lid.by.name.zero" {
         append msgText "Zero LID Retrived. for $msgF(name).%n"
         append msgText "at the end of direct route \"[ArrangeDR $msgF(path)]\""
      }
      "-E-ibdiagpath:direct.route.deadend" {
         ### TODO: check the phrasing ...
         append msgText "Illegal direct route was issued.\n"
         append msgText "The provided direct route passes through a HCA:%n"
         append msgText "$NODE(0,FullName)%n"
         append msgText "(which cannot forward direct route mads)."
      }
      "-E-ibdiagpath:direct.path.no.such.port" {
         append msgText "Illegal direct route was issued.%n"
         append msgText "The following device: $NODE(0,FullName)%n"
         append msgText "does not have port number $msgF(port)."
      }
      "-E-ibdiagpath:link.not.active" {
         append msgText "$NODE(0,FullName) Port=$msgF(port) is in INIT state."
      }
      "-E-ibdiagpath:link.down" {
         append msgText "Illegal route was issued.%n"
         append msgText "Port \#$msgF(port) of:$NODE(0,FullName), is DOWN."
      }
      "-E-ibdiagpath:route.failed" {
         append msgText "Illegal route was issued.%n"
         append msgText "Can not exit through Port \#$msgF(port)"
         append msgText "of the following device:%n$NODE(0,FullName)"
      }
      "-E-ibdiagpath:reached.lid.0" {
         #set noExiting 1
         append msgText "Bad LID: the following device has LID = 0x0000.\n"
         append msgText "$NODE(0,FullName)%n"
         if {[BoolWordInList "+cannotRdPM" $args]} {
            append msgText "Cannot send pmGetPortCounters mads.\n"
         }
         append msgText "$rumSMmsg."
      }
      "-E-ibdiagpath:lid.route.deadend" {
         set port $msgF(port)
         set switchname $NODE(0,FullName)
         append msgText "LID-route deadend was detected.\n"
         append msgText "Entry $msgF(lid) of LFT of the following switch\n"
         append msgText "$switchname\n"
         append msgText "is $port, but port $port leads to\n"
         append msgText "HCA $NODE(0,FullName)."
      }
      "-E-ibdiagpath:lid.route.loop" {
         append msgText "Lid route loop detected: "
         append msgText "when following the LID-route for LID $msgF(lid) from device%n"
         append msgText "$NODE(0,FullName)%n"
      }
      "-E-ibdiagpath:lid.route.dead.end" {
         append msgText "Reached a dead end.: "
         append msgText "when reading LID $msgF(lid) from Fdb table of device%n"
         append msgText "$NODE(0,FullName)."
      }
      "-E-ibdiagpath:read.lft.failure" {
         append msgText "[join $args] - failed $numOfRetries consecutive times."
         # Text ended with "Aborting ibdiagpath."
      }
      "-E-ibdiagpath:fdb.block.unreachable" {
         append msgText "Could not read FDB table: \"$msgF(command)\" "
         append msgText "terminated with errorcode $msgF(errorcode)."
      }
      "-E-ibdiagpath:fdb.bad.value" {
         append msgText "Dead-end trying to reach Lid $msgF(lid)"
      }
      "-E-ibdiagpath:pmGet.failed" {
         set noExiting 1
         append msgText "Could not get PM info:\n"
         append msgText "\"pmGetPortCounters [join $args]\" failed $numOfRetries consecutive times."
      }
      "-I-ibdiagpath:read.lft.header" {
         set from [lindex $args 0]
         set to   [lindex $args 1]
         append msgText "Traversing the path from $from to $to"
         set headerText "LFT Traversal: $from to $to"
      }
      "-I-ibdiagpath:obtain.src.and.dst.lids" {
         append msgText "Obtaining source and destination LIDs:\n"
         append msgText "$msgF(name0) \tLID=$msgF(lid0)\n"
         append msgText "$msgF(name1) \tLID=$msgF(lid1)"
      }
      "-I-ibdiagpath:read.lft.from" {
         append msgText "From: [join $args]"
      }
      "-I-ibdiagpath:read.lft.to" {
         append msgText "To:   [join $args]\n"
      }
      "-I-ibdiagpath:read.pm.header" {
         append msgText "Validating path health"
         set headerText "Path Health Check"
      }
      "-W-ibdiagpath:ardessing.local.node" {
         append msgText "Addressing local node. Only local port will be checked."
      }
      "-I-ibdiagpath:service.level.header" {
         append msgText "Service Level check"
         set headerText "Service Level Check"
      }
      "-I-ibdiagpath:service.level.report" {
         if {[info exists G(argv:service.level)]} {
            if {[lsearch $msgF(suitableSl) $G(argv:service.level)] != -1} {
               append msgText "The provided Service Level: $G(argv:service.level) can be used%n"
            } else {
               for {set i 0} {$i < 2} {incr i} {
                  set name${i} $NODE($i,Name)
                  if {$NODE($i,Name) == ""} {
                     set name${i} $NODE($i,FullName)
                  }
               }
               append msgText "SL${G(argv:service.level)} can not be used in a $msgF(route)%n"
               append msgText "Path is broken between: $name0%nand: $NODE(0,FullName)%n"
               append msgText "SL${G(argv:service.level)} is mapped to VL${msgF(VL)} "
               append msgText "but the maximum VL allowed is VL${msgF(opVL)}"
            }
         } else {
            append msgText "The following SL can be used : $msgF(suitableSl)"
         }
      }

      "-I-write.topology:writing" {
         append msgText "Writing Topology File $G(argv:write.topology)"
      }
      "-E-write.topology:failed" {
         append msgText "Failed to write Topology File $G(argv:write.topology)"
      }
      "-I-topology:matching.header" {
         append msgText "Topology matching results"
         set headerText "Topology Matching Check"
      }
      "-E-topology:localDevice.Duplicated" {
         append msgText "Local Device Guid was duplicated. "
         append msgText "Since local system name was guessed, "
         append msgText "Topology Matching May Failed."
      }
      "-I-topology:matching.note" {
         append msgText "Note that some \"bad\" links and the part of the fabric "
         append msgText "to which they led (in the BFS discovery of the fabric, "
         append msgText "starting at the local node) are not discovered and "
         append msgText "therefore will be reported as \"missing\"."
         if { [llength [array names G bad,paths,*]] == 0 } {
            set msgText ""
            # <- this means don't print the "-I-" prefix
         }
      }
      "-I-topology:matching.perfect" {
         append msgText "The topology defined in $G(argv:topo.file) "
         append msgText "perfectly matches the discovered fabric."
      }
      "-W-topology:matching.bad" {
         append msgText "Many mismatches between the topology defined in "
         append msgText "$G(argv:topo.file) and the discovered fabric:\n"
      }
      "-W-topology:Critical.mismatch" {
         append msgText "Critical mismatch. between the topology defined in "
         append msgText "$G(argv:topo.file) and the discovered fabric:\n"
         append msgText "Topology file names will not be used.\n"
         append msgText "\"$msgF(massage)\""
      }

      "-I-ibdiagnet:report.fab.qualities.header" {
         append msgText "Fabric qualities report"
         set headerText "Fabric Qualities Report"
      }
      "-I-ibdiagnet:report.fab.qualities.report" {
         append msgText "\n[lindex $args 0]"
      }
      "-E-ibdiagnet:report.fab.qualities.errors" {
         set noExiting 1
         set errs [lindex $args 0]
         incr G(status:summary.errs) [expr $errs -1]
         append msgText "Total Qualities Check Errors:$errs"
      }
      "-W-ibdiagnet:report.fab.qualities.warnings" {
         set warns [lindex $args 0]
         incr G(status:summary.warn) [expr $warns -1]
         append msgText "Total Qualities Check Warnings:$warns"
      }
      "-I-ibdiagnet:Checking.bad.guids.lids" {
         append msgText "Checking bad guids"
      }
      "-I-ibdiagnet:SM.header" {
         append msgText "Summary Fabric SM-state-priority"
         set headerText "Subnet Manager Check"
      }
      "-E-ibdiagnet:no.lst.file" {
         set noExiting 1
         append msgText "Fail to find \"$msgF(fileName)\""
      }
      "-E-ibdiagnet:no.SM" {
         append msgText "Missing master SM in the discover fabric"
         set noExiting 1
      }
      "-E-ibdiagnet:many.SM.master" {
         append msgText "Found more then one master SM in the discover fabric"
         set noExiting 1
      }
      "-I-ibdiagnet:SM.report.head" {
         set msgText "  "
         set SMstate [lindex $args 0]
         append msgText "SM - $SMstate"
      }
      "-I-ibdiagnet:SM.report.body" {
         set msgText "    "
         set nodeName [lindex $args 0]
         set priority [lindex $args 1]
         append msgText "$nodeName  priority:$priority"
      }
      "-I-ibdiagnet:check.credit.loops.header" {
         append msgText "Checking credit loops"
         set headerText "Credit Loops Check"
      }
      "-I-ibdiagnet:report.fab.credit.loop.report" {
         append msgText "\n[lindex $args 0]"
      }
      "-E-ibdiagnet:report.fab.credit.loop.errors" {
         set noExiting 1
         set errs [lindex $args 0]
         incr G(status:summary.errs) [expr $errs -1]
         append msgText "Total Credit Loop Check Errors:$errs"
      }
      "-W-ibdiagnet:report.fab.credit.loop.warnings" {
         set warns [lindex $args 0]
         incr G(status:summary.warn) [expr $warns -1]
         append msgText "Total Credit Loop Check Warnings:$warns"
      }
      "-I-ibdiagnet:mgid.mlid.hca.header" {
         append msgText "mgid-mlid-HCAs table"
         set headerText "Multicast Groups Report"     
      }
      "-I-ibdiagnet:bad.guids.header" {
         append msgText "Bad Guids/LIDs Info"
         set headerText "Bad GUIDs/LIDs Check"
      }
      "-I-ibdiagnet:no.bad.guids" {
         append msgText "No bad Guids were found"
      }
      "-I-ibdiagnet:bad.sm.header" {
         append msgText "Bad Fabric SM Info"
         set headerText "SM Info Check"     
      }
      "-I-ibdiagnet:bad.links.header" {
         append msgText "Bad Links Info\n"
         append msgText "-I- Errors have occurred on the following links%n"
         append msgText "(for errors details, look in log file $G(outfiles,.log)):"
         set headerText "Link Errors Check"
      }
      "-I-ibdiagnet:no.bad.paths.header" {
         append msgText "Bad Links Info\n"
         append msgText "-I- No bad link were found"
      }
      "-I-ibdiagnet:bad.link" {
         append putsFlags " -origlen"
         set msgText ""
         append msgText $msgF(link)
      }
      "-I-ibdiagnet:bad.link.errors" {
         append putsFlags " -NoStdout"
         set msgText ""
         append msgText $msgF(errors)
      }
      "-I-ibdiagnet:bad.links.err.types" {
         append putsFlags " -NoStdout -chars \": \""
         set msgText ""
         append msgText "\n Errors types explanation:\n"
         append msgText "   \"noInfo\"  : the link was ACTIVE during discovery "
         append msgText "but, sending MADs across it failed $numOfRetries consecutive times\n"
         append msgText "   \"badPMs\"  : one of the Error Counters of the link "
         append msgText "has values higher than predefined thresholds.\n"
         append msgText "   \"madsLost\": $G(var:badpath.maxnErrors) MADs were "
         append msgText "dropped on the link (drop ratio is given).\n"
      }
      "-I-ibdiagnet:bad.link.width.header" {
         append msgText "Links With links width != $G(argv:link.width) (as set by -lw option)"
         set headerText "Specific Link Width Check"
      }
      "-I-ibdiagnet:no.bad.link.width" {
         append msgText "No unmatched Links (with width != $G(argv:link.width)) were found"
      }
      "-W-ibdiagnet:report.links.width.state" {
         set dontTrimLine 1
         append msgText "link with PHY=$msgF(phy) found at direct path \"$PATH(1)\"\n"
         append msgText "From: a $NODE(0,FullType,Spaces) $NODE(0,Name,Spaces)"
         append msgText " PortGUID=$NODE(0,PortGUID) Port=[lindex [split $PATH(1) ,] end]\n"
         append msgText "To:   a $NODE(1,FullType,Spaces) $NODE(1,Name,Spaces)"
         append msgText " PortGUID=$NODE(1,PortGUID) Port=$NODE(1,EntryPort)"
      }
      "-I-ibdiagnet:bad.link.speed.header" {
         append msgText "Links With links speed != $G(argv:link.speed) (as set by -ls option)"
         set headerText "Specific Link Speed Check"
      }
      "-I-ibdiagnet:no.bad.link.speed" {
         append msgText "No unmatched Links (with speed != $G(argv:link.speed)) were found"
      }
      "-W-ibdiagnet:report.links.speed.state" {
         set dontTrimLine 1
         append msgText "link with SPD=$msgF(spd) found at direct path \"$PATH(1)\"\n"
         append msgText "From: a $NODE(0,FullType,Spaces) $NODE(0,Name,Spaces)"
         append msgText " PortGUID=$NODE(0,PortGUID) Port=[lindex [split $PATH(1) ,] end]\n"
         append msgText "To:   a $NODE(1,FullType,Spaces) $NODE(1,Name,Spaces)"
         append msgText " PortGUID=$NODE(1,PortGUID) Port=$NODE(1,EntryPort)"
      }
      "-I-ibdiagnet:bad.link.logic.header" {
         append msgText "Links With Logical State = INIT"
         set headerText "Link State Active Check"
      }
      "-I-ibdiagnet:no.bad.link.logic" {
         append msgText "No bad Links (with logical state = INIT) were found"
      }
      "-I-ibdiagnet:pm.counter.report.header" {
         append msgText "PM Counters Info"
         set headerText "Performance Counters Report"
      }
      "-I-ibdiagnet:no.pm.counter.report" {
         append msgText "No illegal PM counters values were found"
      }
      "-W-ibdiagnet:bad.pm.counter.report" {
         append msgText "$NODE(0,FullNamePort_Last)%n"
         append msgText "      Performance Monitor counter"
         append msgText "[string repeat " " [expr [GetLengthMaxWord $G(var:list.pm.counter)] - 27]] : "
         append msgText "Value"

         foreach err $msgF(listOfErrors) {
            append msgText %n
            regexp {([^ =]*)=(.*)} $err . pmCounter pmTrash
            append msgText "      $pmCounter"
            append msgText "[string repeat " " [expr [GetLengthMaxWord $G(var:list.pm.counter)] - [string length $pmCounter]]] : "
            append msgText $pmTrash
         }
      }
      "-I-ibdiagnet:external.flag.execute.header" {
         append msgText "Executing external option: $msgF(flag)"
         set headerText "External Options"
      }
      "-I-ibdiagnet:external.flag.execute.node.report" {
         append msgText "$NODE(0,FullName_Last)%n"
         append msgText "      [join $msgF(report) %n]"
      }
      "-I-ibdiagnet:external.flag.execute.no.report" {
         append msgText "Nothing to report"
      }


      "-W-ibdiagnet:local.link.in.init.state" {
         append msgText "The local link is in INIT state, no PM counter reading could take place"
      }
      "-W-ibdiagnet:report.links.init.state" {
         set dontTrimLine 1
         append msgText "link with LOG=INI found at direct path \"$PATH(1)\"\n"
         append msgText "From: a $NODE(0,FullType,Spaces) $NODE(0,Name,Spaces)"
         append msgText " PortGUID=$NODE(0,PortGUID) Port=[lindex [split $PATH(1) ,] end]\n"
         append msgText "To:   a $NODE(1,FullType,Spaces) $NODE(1,Name,Spaces)"
         append msgText " PortGUID=$NODE(1,PortGUID) Port=$NODE(1,EntryPort)"
      }
      "-W-report:one.hca.in.fabric" {
         append msgText "The fabric has only one HCA. No fabric qualities report is issued."
      }
      "-W-ibdiagnet:PKeys.noNodeInfo" {
         append msgText "failed to get node info for node:[lindex $args 0]"
      }
      "-E-ibdiagnet:PKeys.getPkey" {
         set drPath  [lindex $args 0]
         set portNum [lindex $args 1]
         set block   [lindex $args 2]
         append msgText "failed to get PKeyTable for drPath:$drPath port:$portNum block:$block"
      }
      "-W-ibdiagnet:PKeys.in.out.not.same" {
         set nodeName  [lindex $args 0]
         set inb    [lindex $args 1]
         set outb   [lindex $args 2]
         append msgText "Missmatching In-Bound ($inb) and Out-Bound ($outb) PKey enforcement on switch $nodeName"
      }
      "-E-ibdiagnet:PKeys.noSwitchInfo" {
         set guid    [lindex $args 0]
         set drPath  [lindex $args 1]
         append msgText "failed to get SwitchInfo from drPath:$drPath node:$guid"
      }
      "-W-ibdiagnet:PKeys.switch.missing.pkey" {
         set name   [lindex $args 0]
         set pkey   [lindex $args 1]
         append msgText "Missing PKey:[format 0x%04x $pkey] on remote switch of node:$name"
      }
      "-W-ibdiagnet:PKeys.switch.part.pkey" {
         set name   [lindex $args 0]
         set pkey   [lindex $args 1]
         append msgText "Only partial PKey:[format 0x%04x $pkey] allowed by remote switch of node:$name"
      }
      "-I-ibdiagnet:PKeys.report.header" {
         append msgText "Fabric Partitions Report (see $G(var:tool.name).pkey for a full hosts list)"
         set headerText "Partitions Check"
      }
      "-I-ibdiagnet:PKeys.Group" {
         set base [format 0x%04x [lindex $args 0]]
         set hosts [lindex $args 1]
         set full  [lindex $args 2]
         set part  [lindex $args 3]
         append msgText "   PKey:$base Hosts:$hosts full:$full partial:$part"
      }
      "-I-ibdiagpath:PKeys.report.header" {
         append msgText "Path Partitions Report"
         set headerText "Path Partitions Check"
      }
      "-I-ibdiagpath:PKeys.src.pkeys" {
         foreach {nodeName portNum pkeys} $args {break}
         append msgText "Source $nodeName Port $portNum PKeys:$pkeys"
      }
      "-I-ibdiagpath:PKeys.dst.pkeys" {
         set node [lindex $args 0]
         set pkeys [lindex $args 1]
         append msgText "Destination $node PKeys:$pkeys"
      }
      "-E-ibdiagpath:PKeys.FailNodeInfo" {
         append msgText "Failed to obtain NodeInfo for directed route:[lindex $args 0]. Abortig PKeys Check."
      }
      "-E-ibdiagpath:PKeys.FailPortInfo" {
         append msgText "Failed to obtain PortInfo for directed route:[lindex $args 0]. Abortig PKeys Check."
      }
      "-V-ibdiagpath:PKeys.portPkeys" {
         foreach {nodeName portNum dir pkeys} $args {break}
         if {$dir == "in"} {
            set x "to"
         } else {
            set x "from"
         }
         append msgText "going:$dir $x node:$nodeName port:$portNum PKeys:$pkeys"
      }
      "-W-ibdiagpath:PKeys.blockOnPath" {
         foreach {nodeName dir pkey} $args {break}
         append msgText "PKey:[format 0x%04x $pkey] was blocked on $dir pin of $nodeName"
      }
      "-I-ibdiagpath:PKeys.path.shared" {
         append msgText "Path shared PKeys: [lindex $args 0]"
      }
      "-E-ibdiagpath:PKeys.path.noShared" {
         append msgText "No shared PKeys found on Path! Nodes can not communicate!"
      }
      "-I-ibdiagnet:ipoib.header" {
         append msgText "IPoIB Subnets Check"
         set headerText "IPoIB Subnets Check"
      }
      "-I-ipoib.subnet" {
         foreach {IPV pkey gMtu gRate gSL gPKey gQKey} $args {break}
         set pkey [format 0x%04x $pkey]
         set mtu [string range "[PathRecMTUCodeText $gMtu]Byte" 1 end]
         set rate [string range "[PathRecRateCodeText $gRate]Gbps" 1 end]
         set qkey [format 0x%08x $gQKey]
         set sl   [format 0x%02x $gSL]
         append msgText "Subnet: IPv$IPV PKey:$pkey QKey:$qkey MTU:$mtu rate:$rate SL:$sl"
      }
      "-W-ipoib.bad.pkey" {
         foreach {gPKey pkey} $args {break}
         set pk1 [format 0x%04x $gPKey]
         set pk2 [format 0x%04x $pKey]
         append msgText "Missmatching Multicast Group PKey:$pk1 and MGID Pkey:$pk2"
      }
      "-W-ibdiagnet.ipoib.noMemers" {
         append msgText "No members found for group"
      }
      "-E-ipoib.ilegalRate" {
         set name [lindex $args 0]
         append msgText "Illegal speed/width on port:$name"
      }
      "-W-ipoib.cantJoin" {
         foreach {name rate gRate} $args {break}
         set rateStr [string range [PathRecRateCodeText $rate] 1 end]
         set gRateStr [string range [PathRecRateCodeText $gRate] 1 end]
         append msgText "Port $name can not join due to rate:${rateStr}Gbps < group:${gRateStr}Gbps"
      }
      "-W-ibdiagnet.ipoib.rateToLow" {
         foreach {minRate gRate} $args {break}
         set minRateStr [string range [PathRecRateCodeText $minRate] 1 end]
         set gRateStr [string range [PathRecRateCodeText $gRate] 1 end]
         append msgText "Suboptimal rate for group. Lowest member rate:${minRateStr}Gbps > group-rate:${gRateStr}Gbps"
      }
      "-I-ibdiagpath:ipoib.header" {
         append msgText "IPoIB Path Check"
         set headerText "Path IPoIB Check"
      }
      "-E-ibdiagpath.ipoib.noGroups" {
         set noExiting 1
         append msgText "No IPoIB Subnets found on Path! Nodes can not communicate via IPoIB!"
      }
      "-I-ibdiagpath:qos.report.header" {
         append msgText "QoS on Path Check"
         set headerText "QoS on Path Check"
      }
      "-V-ibdiagpath.qos.atNode" {
         foreach {name inPort outPort} $args {break}
         append msgText "At node:$name from-port:$inPort out-port:$outPort"
      }
      "-E-ibdiagpath:qos.FailPortInfoOpVLs" {
         append msgText "Failed to get PortInfo for path:$args"
      }
      "-E-ibdiagpath:qos.FailSL2VL" {
         append msgText "Failed to get SL2VLTable for path:$args"
      }
      "-E-ibdiagpath:qos.FailVLArb" {
         append msgText "Failed to get VLArbTable for path:$args"
      }
      "-W-ibdiagpath:qos.vlaOverOpVLs" {
         foreach {name port entries opVLs HL} $args {break}
         set lastVL [expr $opVLs - 1]
         if {$lastVL == 15} {set lastVL 14}
         append msgText "VLArbTable$HL Entries:$entries VL > $lastVL at node:$name port:$port"
      }
      "-W-ibdiagpath:qos.blockedVLs" {
         foreach {name outPort blocked} $args {break}
         append msgText "Blocked VLs:$blocked at node:$name port:$outPort"
      }
      "-W-ibdiagpath:qos.sl2vlOORange" {
         foreach {name inPort outPort opVLs sls} $args {break}
         set lastVL [expr $opVLs - 1]
         if {$lastVL == 15} {set lastVL 14}
         append msgText "SLs:$sls mapped to VL > $lastVL at node:$name in-port:$inPort out-port:$outPort"
      }
      "-W-ibdiagpath:qos.blockedSL" {
         foreach {name inPort outPort sls} $args {break}
         append msgText "SLs:$sls are blocked due to VLArb node:$name in-port:$inPort out-port:$outPort"
      }
      "-I-ibdiagpath:qos.pathSLs" {
         append msgText "The following SLs can be used:[lindex $args 0]"
      }
      "-E-ibdiagpath:qos.noPathSLs" {
         append msgText "No SLs can be used. No communication from source to destination possible !"
      }
      "-I-exit:\\r" {
         set msgText ""
         # <- this means don't print the "-I-" prefix
         append msgText "%n"
      }
      "-I-done" {
         SummaryMessage
         PutsIn80Chars " "
         append msgText "Done. Run time was [expr [clock seconds] - $args] seconds."
      }

      "-F-Fatal.header" {
         append msgText "Fatal fabric condition found.%n-F- Please fix the above errors and rerun."
      }


      "-V-mad:sent"  {
         if {$dontShowMads} { return }
         append putsFlags " -nonewline"
         append msgText "running $msgF(command) ..."
      }
      "-V-mad:received" {
         if {$dontShowMads} { return }
         set msgText ""
         if {[info exists msgF(status)]} {
            append msgText "status = $msgF(status) (after $msgF(attempts) attempts)"
         } else {
            append msgText "done (after $msgF(attempts) attempts)"
         }
      }
      "-V-badPathRegister" {
         append msgText "Bad path found: failure = $msgF(error). "
         append msgText "When running: $msgF(command)."
      }
      "-V-discover:start.discovery.header"   {
         append msgText "Starting subnet discovery"
      }
      "-V-discover:end.discovery.header"  {
         set nodesNum [llength [array names G "NodeInfo,*"]]
         set swNum    [llength [array names G "PortInfo,*:0"]]
         append msgText "Subnet discovery finished.%n"
         append msgText "$nodesNum nodes ($swNum Switches & [expr $nodesNum - $swNum] CA-s) discovered"
      }
      "-V-ibdiagnet:bad.links.info" {
         foreach entry [array names G bad,paths,*] {
            append msgText "\nFailure(s) for direct route [lindex [split $entry ,] end] : "
            if { [llength $G($entry)] == 1 } {
               append msgText "[join $G($entry)]"
            } else {
               append msgText "\n      [join $G($entry) "\n      "]"
            }
         }
      }
      "-V-discover:long.paths" {
         PutsIn80Chars " "
         append msgText "Retrying discovery multiple times (according to the -c flag) ... "
      }
      "-V-ibdiagnet:bad.links.detected" {
         PutsIn80Chars " "
         append msgText "Searching for bad link(s) on direct route \{[ArrangeDR $msgF(path)]\} ..."
      }
      "-V-ibdiagnet:incremental.bad.links" {
         append msgText "Sending MADs over increments of the direct route \{[ArrangeDR $msgF(path)]\}"
      }
      "-V-ibdiagpath:pm.value" {
         append msgText "PM [join $args]"
      }
      "-V-outfiles:.lst"   {
         PutsIn80Chars " "
         append msgText "Writing file $G(outfiles,.lst) "
         append msgText "(a listing of all the links in the fabric)"
      }
      "-V-outfiles:.fdbs"  {
         PutsIn80Chars " "
         append msgText "Writing file $G(outfiles,.fdbs)"
         append msgText "(a dump of the unicast forwarding tables of the fabric switches)"
      }
      "-V-outfiles:.mcfdbs"   {
         PutsIn80Chars " "
         append msgText "Writing file $G(outfiles,.mcfdbs)"
         append msgText "(a dump of the multicast forwarding tables of the fabric switches)"
      }


      "-I-ibping:results" {
         set pktFailures   $msgF(failures)
         set pktTotal   $G(argv:count)
         set pktSucces  [expr $pktTotal - $pktFailures]
         set pktSuccesPrc  [expr ( round ( ($pktSucces / $pktTotal)*10000 ) ) / 100.0 ]
         set pktFailuresPrc   [expr 100 - $pktSuccesPrc]
         set avrgRoundTrip $msgF(time)
         PutsIn80Chars " "
         append msgText "ibping: pass:  $pktSucces $pktSuccesPrc%, "
         append msgText "failed: $pktFailures $pktFailuresPrc%, "
         append msgText "average round trip: $avrgRoundTrip ms"
      }
      "-V-ibping:ping.result" {
         # "12:38:16 010020 lid=2: seq=1 time=0.365 ms"

         set cc [clock clicks]
         set us [string range $cc [expr [string length $cc] - 6] end]

         catch { set address "lid=$G(argv:lid.route)" }
         catch { set address "name=$G(argv:by-name.route)" }
         catch { set address "direct_route=\"[split $G(argv:direct.route) ,]\"" }

         set seqLen [string length $G(argv:count)]
         if { $msgF(retry) == 1 } { puts "\n$bar\n-V- ibping pinging details\n$bar" }
         set seq [string range "$msgF(retry)[Bar " " $seqLen]" 0 [expr $seqLen -1] ]

         if { $msgF(time) == "failed" } {
            set result "failed"
         } else {
            set result "time=[string range "[expr $msgF(time) / 1000.0]0000" 0 4] ms"
         }

         append msgText "[clock format [clock seconds] -format %H:%M:%S] "
         append msgText "$us $address seq=$seq $result"
      }

      "-F-crash:failed.build.lst" {
         set noExiting 1
         append msgText "IBdiagnet Failed to build $G(outfiles,.lst). Contact Mellanox officials"
      }
      "-F-crash:failed.parse.lst" {
         append msgText "IBFabric_parseSubnetLinks Failed to parse $G(outfiles,.lst)"
      }
      "-F-crash:failed.parse.fdbs" {
         append msgText "IBFabric_parseFdbFile Failed to parse $G(outfiles,.fdbs)"
      }
      "-F-crash:failed.parse.mcfdbs" {
         append msgText "IBFabric_parseMCFdbFile Failed to parse $G(outfiles,.mcfdbs)"
      }
      "-E-argv.pkg.external.flag" {
         append msgText "Illegal argument: I${llegalValMsg}: $msgF(value)%n"
         append msgText "For legal value please contact package owner"
      }
   }

   ##################################################
   ## Summary generation
   if {$msgType == "-E-"} {
      incr G(status:summary.errs)
   } elseif {$msgType == "-W-"} {
      incr G(status:summary.warn)
   }

   if {[regexp ".header" $msgCode] && $headerText != ""} {
      if {$G(status:summary.headr) != 0} {
         set prevHeadr $G(status:summary.headr)
         set warns $G(status:summary.warn)
         set errs  $G(status:summary.errs)
         append G(status:summary.text) \
            [format "    %-40s %-6d %-6d\n" $prevHeadr $errs $warns]
      }
      set G(status:summary.errs) 0
      set G(status:summary.warn) 0
      set G(status:summary.headr) $headerText
   }

   ### Writing out the message
   set msgText [split $msgText \n]
   if {[regexp ".header" $msgCode]} {
      set msgText [concat [list ""] [list $bar] [linsert $msgText 1 $bar]]
   }

   if { ($msgType == "-E-") || ($msgType == "-F-") } {
      if { ! [info exists noExiting] } {
         puts ""
         switch $msgSource {
            "discover" {
               set Exiting "Aborting discovery"
            }
            "ibdiagpath" {
               set Exiting "Aborting route tracing"
            }
            default {
               set Exiting "Exiting"
            }
         }
         switch -exact -- $msgSource {
            "ibis" {
               set exitStatus $G(status:ibis.init)
            }
            "argv" {
               set showSynopsys 1
               set exitStatus $G(status:illegal.flag.value)
            }
            "localPort" {
               set exitStatus $G(status:root.port.get)
            }
            "discover" {
               set exitStatus $G(status:discovery.failed)
            }
            "ibdiagpath" {
               set exitStatus $G(status:discovery.failed)
            }
            "topology" {
               set exitStatus $G(status:topology.failed)
            }
            "loading" {
               set exitStatus $G(status:topology.failed)
            }
            "crash" {
               set exitStatus $G(status:crash)
            }
            default {
               set exitStatus $G(status:illegal.flag.value)
            }
         }
      }
   }
   regsub -all {%n} "[join $msgText \n]" "\n" msgText
   ### DontTrimLine
   if {[info exists dontTrimLine]} {
      PutsIn80Chars $msgText $putsFlags -length 160
   } else {
      PutsIn80Chars $msgText $putsFlags
   }
   if {[info exists exitStatus]} {
      puts "    $Exiting.\n"
      if {[info exists showSynopsys]} {
         showHelpPage -sysnopsys
         puts ""
      }
      catch { close $G(logFileID) }
      exit $exitStatus
   }
   return
}
##############################

##############################
#  NAME         RequirePackage   
#  FUNCTION require the available packages for device specific crRead/crWrite
#  RESULT       ammm... the available packages are required
proc RequirePackage {} {
   global G PKG_FLAGS InfoArgv
   ### Require Tcl and ibdm packages
   if {[string compare [package provide Tcl] 8.4] < 0} {
      inform "-E-loading:cannot.use.current.tcl.package" -version [package provide Tcl]
   }

   set pkgList [lsearch -inline -all [package names] ibdiag_*]

   foreach pkg $pkgList {
      scan [split $pkg _] {%s %s} . pkgName
      ## Package - Test1.0: package name
      if {![info exists pkgName]} {
         inform "-W-loading:cannot.load.package" -package $pkg -error "Illegal name"
         continue
      } else {
         package require $pkg
      }

      set listProcs [info procs ${pkg}::*]
      set criticalErr 0
      ## Package - Test2.0: package must include: ::${pkg}::GetPkg_Flags/GetPkg_FlagInfo
      foreach procName "::${pkg}::GetPkg_Flags ::${pkg}::GetPkg_FlagInfo" {
         if {[lsearch $listProcs $procName] == -1} {
            inform "-W-loading:cannot.load.package" -package $pkg \
               -error "The following procedure is missing: $procName"
            set criticalErr 1
            break
         }
      }
      if {$criticalErr} {
         continue
      }

      ## Package - Test3-Test4: each package flags
      set listOfFlags [::${pkg}::GetPkg_Flags]
      foreach tmpFlag $listOfFlags {
         # Package - Test3: Deal with duplicate flags
         if {[info exists PKG_FLAGS(data:list.flags)] } {
            if {[lsearch $PKG_FLAGS(data:list.flags) $tmpFlag] != -1} {
               inform "-W-loading:cannot.load.package" -package $pkg \
                  -error "The following flag: $tmpFlag allready provided"
               set $criticalErr 1
            }
         }
         if {$criticalErr} {
            break
         }
         # Package - Test3.0-Test3.1: Flag name format
         set flgName [string trimleft $tmpFlag -]
         set tmpPkgName [string range $flgName 0 [expr [string length $pkgName] - 1 ]]
         if {$tmpPkgName != $pkgName} {
            inform "-W-loading:cannot.load.package" -package $pkg \
               -error "The following flag is illegal: $tmpFlag"
            set criticalErr 1
            continue
         }

         # Package - Test4.0: each flag must return 1 for illegal flags
         set tmpFlagInfo [${pkg}::GetPkg_FlagInfo $tmpFlag]
         if {$tmpFlagInfo == 1} {
            inform "-W-loading:cannot.load.package" -package $pkg \
               -error "The following flag: $tmpFlag return with 0 from GetPkg_FlagInfo"
            set criticalErr 1
            continue
         }

         # Package - Test4.0.1-Test4.1: Try to add a package flag
         switch [UpdateInfoArgv_Pkg [${pkg}::GetPkg_FlagInfo $tmpFlag] $tmpFlag] {
            0 {
            }
            -1 {
               inform "-W-loading:cannot.load.package" -package $pkg \
                  -error "The following flag: $tmpFlag unsupported tools were given"
               set criticalErr 1
               continue
            }
            1 {
               ### DZ: no entries are given
            }
         }

         # Package - Test4.2: each flag must have procedure.info entry in GetPkg_FlagInfo
         set procInfoIndx [lsearch -regexp $tmpFlagInfo "$tmpFlag,proc.info" ]
         if {$procInfoIndx == -1} {
            inform "-W-loading:cannot.load.package" -package $pkg \
               -error "The following flag: $tmpFlag doesn't have a matching proc"
            set criticalErr 1
            continue
         }

         # Package - Test4.3: run/check/dump procedures
         set procsNames [lindex [lindex $tmpFlagInfo $procInfoIndx] end]
         regexp {^([^.]*).([^.]*).([^.]*).([^.]*)} $procsNames . run check dump parse
         if {[llength $run] == 0} {
            inform "-W-loading:cannot.load.package" -package $pkg \
               -error "The following flag: $tmpFlag doesn't have a running procedure"
            set criticalErr 1
            continue
         }
         foreach procName "run check dump parse" {
            if {[subst $$procName] == ""} {
               continue
            }
            if {[lsearch $listProcs ::${pkg}::[subst $$procName]] == -1} {
               inform "-W-loading:cannot.load.package" -package $pkg \
                  -error "The following flag: $tmpFlag support a proc which doesn't exists: ::${pkg}::[subst $$procName]"
               set criticalErr 1
               continue
            }
         }
         if {$criticalErr} {
            continue
         }
         set PKG_FLAGS(data:$tmpFlag.run) ::${pkg}::$run
         set PKG_FLAGS(data:$tmpFlag.check) ::${pkg}::$check
         set PKG_FLAGS(data:$tmpFlag.dump) ::${pkg}::$dump
         set PKG_FLAGS(data:$tmpFlag.parse) ::${pkg}::$parse

         # Package - Test4.4: legal flag,error message
         set errorInfoIndx [lsearch -regexp $tmpFlagInfo "$tmpFlag,error" ]
         if {$errorInfoIndx == -1} {
            inform "-W-loading:cannot.load.package" -package $pkg \
               -error "The following flag: $tmpFlag doesn't have an error message"
            set criticalErr 1
            continue
         } else {
            set errorMassage [lrange [lindex $tmpFlagInfo $errorInfoIndx] 1 end]
            if {![string equal $errorMassage -E-argv.pkg.external.flag]} {
               inform "-W-loading:cannot.load.package" -package $pkg \
                  -error "The following flag: $tmpFlag have an illegal error message: $errorMassage"
               set criticalErr 1
               continue
            }
         }

         # Package - Test4.5: legal flag,name val
         set nameInfoIndx [lsearch -regexp $tmpFlagInfo "$tmpFlag,name" ]
         if {$nameInfoIndx == -1} {
            inform "-W-loading:cannot.load.package" -package $pkg \
               -error "The following flag: $tmpFlag doesn't have a name entry"
            set criticalErr 1
            continue
         } else {
            set nameVal [lrange [lindex $tmpFlagInfo $nameInfoIndx] 1 end]
            if {![string equal $nameVal $flgName]} {
               inform "-W-loading:cannot.load.package" -package $pkg \
                  -error "The following flag: $tmpFlag have an illegal name val: $nameVal"
               set criticalErr 1
               continue
            }
         }

         set toolNameIndx [lsearch $tmpFlagInfo "*,tool.name *"]
         set listOfTools [lrange [lindex $tmpFlagInfo $toolNameIndx] 1 end]
         foreach toolName $listOfTools {
            UpToolsFlags [string trimleft $tmpFlag -] $toolName
         }
         lappend PKG_FLAGS(data:list.flags) $tmpFlag
      }
      if {$criticalErr} {
         continue
      }
      foreach toolName $listOfTools {
         UpToolsFlags . $toolName
      }
   }
}

proc RequireIBDM {} {
   global G env
   if {![BoolWordInList load_ibdm $G(argv:skip.checks)]} {
      if {[catch { package require ibdm } errMsg]} {
         inform "-E-loading:cannot.load.package.ibdm" -errMsg $errMsg
      }

      if {[info commands ibdmFindRootNodesByMinHop] == ""} {
         inform "-E-loading:cannot.use.current.ibdm.package" -version [package provide ibdm]
      }

      if {[info commands ibdmUseInternalLog] == ""} {
         inform "-E-loading:cannot.use.current.ibdm.package" -version [package provide ibdm]
      }

		set env(IBDM_IBNL_PATH) [file join $G(argv:out.dir) ibdiag_ibnl]

      if {[info exists G(argv:topo.file)] && (![BoolWordInList load_ibdm $G(argv:skip.checks)])} {
         set G(IBfabric:.topo) [new_IBFabric]
         if {[IBFabric_parseTopology $G(IBfabric:.topo) $G(argv:topo.file)]} {
            inform "-E-parse.topology"
         }
      }
   } else {
      inform "-W-loading:cannot.load.package.ibdm"
   }

   return
}
##############################

##############################
proc showHelpPage { args } {
   global G InfoArgv

   ##############################
   ### ibdiagnet help page
   ##############################
   set helpPage(ibdiagnet) \
      "DESCRIPTION
      ibdiagnet scans the fabric using directed route packets and extracts all the
      available information regarding its connectivity and devices.
      It then produces the following files in the output directory defined by the
      -o option (see below):
            ibdiagnet.lst    - List of all the nodes, ports and links in the fabric
            ibdiagnet.fdbs   - A dump of the unicast forwarding tables of the fabric
                               switches
            ibdiagnet.mcfdbs - A dump of the multicast forwarding tables of the fabric
                               switches
            ibdiagnet.masks  - In case of duplicate port/node Guids, these file include
                               the map between masked Guid and real Guids
            ibdiagnet.sm     - A dump of all the SM (state and priority) in the fabric
            ibdiagnet.pm     - In case -pm option was provided, this file contain a dump
                               of all the nodes PM counters
            ibdiagnet.db     - A dump of the internal subnet database. This file can be loaded
                               in later runs using the -load_db option
            In addition to generating the files above, the discovery phase also checks for
            duplicate node/port GUIDs in the IB fabric. If such an error is detected, it
            is displayed on the standard output.
            After the discovery phase is completed, directed route packets are sent
            multiple times (according to the -c option) to detect possible problematic
            paths on which packets may be lost. Such paths are explored, and a report of
            the suspected bad links is displayed on the standard output.
            After scanning the fabric, if the -r option is provided, a full report of the
            fabric qualities is displayed.
            This report includes:
            SM report
            Number of nodes and systems
            Hop-count information:
            maximal hop-count, an example path, and a hop-count histogram
            All CA-to-CA paths traced
            Credit loop report
            mgid-mlid-HCAs matching table
            Note: In case the IB fabric includes only one CA, then CA-to-CA paths are not
            reported.
            Furthermore, if a topology file is provided, ibdiagnet uses the names defined
            in it for the output reports.

            ERROR CODES
            1 - Failed to fully discover the fabric
            2 - Failed to parse command line options
            3 - Failed to interact with IB fabric
            4 - Failed to use local device or local port
            5 - Failed to use Topology File
            6 - Failed to load required Package"

   #   The number of retries of sending a specific packet is given by the -f option (default = 3).
   ##############################
   ### ibdiagpath help page
   ##############################
   set helpPage(ibdiagpath) \
      "DESCRIPTION
            ibdiagpath traces a path between two end-points and provides information
            regarding the nodes and ports traversed along the path. It utilizes device
            specific health queries for the different devices along the traversed path.
            The way ibdiagpath operates depends on the addressing mode used on the command
            line. If directed route adressing is used, the local node is the source node
            and the route to the destination port is known apriori.
            On the other hand, if LID route (or by-name) addressing is imployed,
         then the source and destination ports of a route are specified by their LIDs
            (or by the names defined in the topology file). In this case, the actual path
            from the local port to the source port, and from the source port to the
            destination port, is defined by means of Subnet Management Linear Forwarding
            Table queries of the switch nodes along those paths. Therefore, the path
            cannot be predicted as it may change.
            The tool allows omitting the source node, in which case the local port on the
            machine running the tool is assumed to be the source.
            Note: When ibdiagpath queries for the performance counters along the path between
            the source and destination ports, it always traverses the LID route, even if a
            directed route is specified. If along the LID route one or more links are not
            in the ACTIVE state, ibdiagpath reports an error.

            ERROR CODES
            1 - The path traced is un-healthy
            2 - Failed to parse command line options
            3 - More then 64 hops are required for traversing the local port to the \"Source\" port and then to the \"Destination\" port.
            4 - Unable to traverse the LFT data from source to destination
            5 - Failed to use Topology File
            6 - Failed to load required Package"
   #   The number of retries of sending a specific packet is given by the -f option (default = 3).

   ##############################
   ### ibping help page
   ##############################
   set helpPage(ibping) \
      "DESCRIPTION
            ibping pings a target port by sending SM_PortInfo mad request to the target port.
            The target port is specified by means of either its LID,
         a direct route to it - a sequence of a output ports through which the packet should be forwarded,
         or its name (the latter - provided that the fabric's topoloy is given).
            If the target node is specified by a direct route, the packets will be sent by direct route,
         otherwise (lid or name addressing) they will be sent by LID route.
            The total number of packets sent is defined by the -c option (100 by default).
            After sending the desired number of packets ibping reports back
            the accumulated number of failures and successful responses.
            If the -v option is defined a verbose line is printed for each packet sent,
         containing the time sent, and the trip total time or failure.

            ERROR CODES
            1 - Some packets failed to return
            2 - Failed to parse command line options
            3 - Given name does not exist or some topology required parameters were not provided
            4 - Failed to bind to the driver"

   ##############################
   ### ibcfg help page
   ##############################
   set helpPage(ibcfg) \
      "DESCRIPTION
            ibcfg provides user interface for device level configuration.
            It runs in two modes: query mode (-q) or configuration mode (-c).
            The query mode provides back a list of the specific device commands and their parameters.
            The configuration mode requires the name of the command to be executed and its parameters.
            To support \"Device Specific\" configuration options the implementation of this command
            relies on vendor provided modules.
            With this architecture each device has its own set of commands provided by the device manufacturer.
            Please see the set of commands that will be available for each Mellanox device at appendix A.
            This utility also supports controlling local devices accessible through the local PCI bus or I2C.
            To configure a local device the -m flag should be provided following an MST device designator.

            ERROR CODES
            1 - Fail to execute the command
            2 - No such command for the given device
            3 - Fail to find the given device"

   ##############################
   ### ibmad help page
   ##############################
   set helpPage(ibmad) \
      "DESCRIPTION
            ibmad is a generic mad injector supporting many types of attributes.
            It works in two modes: query and injection.
            The query mode denoted by the -q option provides the list of attributes supported
            and their detailed list of fields or required modifier values:
            if an attribute is provided then the list of the attribute fields and modifiers is printed.
               Otherwise the list of attributes is printed.
                  The injection mode requires a method and an attribute options and optional list
                  of field name and values to fill into the mad.
                  All fields not being provided are filled with zeros.

                  ATTRRIBUTES:
                  The following attributes are supported:
                  SMPs:
                  SM_NodeInfo, SM_PortInfo, SM_SwitchInfo, SM_NodeDescription,
                  SM_LFTBlock, SM_MFTBlock, SM_GUIDInfo, SM_PKeyTable,
         SM_SLVLTable and SM_VLArbTable, SM_Notice.
            Performance Monitoring Queries:
            PM_PortCounters
            Vendor Specific:
            VS_CfgReg, VS_CfgRegVL15

            ERROR CODES
            -1 - Fail to find target device
            -2 - Fail to parse command line (might be wrong attribute method field...)
            <1-N> - Remote mad status"

   ##############################
   ### ibsac help page
   ##############################
   set helpPage(ibsac) \
      "DESCRIPTION
            ibsac sends SA queries.
            The supported attributes and their fields
            are provided in the \"query\" mode (-q).

            ERROR CODES
            -1 - Fail to find target device
            -2 - Fail to parse command line (might be wrong attribute method field)
            <1-N> - Remote mad status"

   ##############################
   ### ibdiagui help page
   ##############################
   set helpPage(ibdiagui) \
      "DESCRIPTION
            ibdiagui is a GUI wrapper for ibdiagnet.
            Its main features:
            1. Display a graph of teh discovered fabric (with optional names annotattion)
            2. Hyperlink the ibdiagnet log to the graph objects
            3. Show each object properties and object type specific actions
            on a properties pannel."

   # OPTIONS
   # -<field-i> <val-i>: specific attribute field and value. Automatically sets the component mask bit.
   ##############################

   set onlySynopsys [BoolWordInList "-sysnopsys" $args]
   # NAME
   if { ! $onlySynopsys } {
      puts "NAME\n  $G(var:tool.name)"
   }

   # SYNOPSIS
   set SYNOPSYS "SYNOPSYS\n  $G(var:tool.name)"
   set OPTIONS "OPTIONS"
   foreach item [GetToolsFlags $G(var:tool.name)] {
      if { $item == ";" } {
         append SYNOPSYS "\n\n  $G(var:tool.name)"
         continue;
      }
      if { $item == "." } {
         append SYNOPSYS "\n    "
         continue;
      }

      set synopsysFlags ""
      set mandatory [regsub -all {[()]} $item "" item]
      foreach flag [split $item |] {
         set flagNparam "-$flag"
         catch { append flagNparam " <$InfoArgv(-$flag,param)>" }
         set flagNdesc "$flagNparam:"
         if {[catch { append flagNdesc " $InfoArgv(-$flag,desc)" }]} {
            append flagNdesc " "
         }
         catch {
            if { ( [set defVal $InfoArgv(-$flag,default)] != "" ) \
                    && ( ! [BoolWordInList $flag "i v"] ) } {
               append flagNdesc " (default = $defVal)"
            }
         }
         if {[regexp {\(} $item]} { ; # only mandatory flags have optional flags
            catch { append flagNparam " \[$InfoArgv(-$flag,optional)\]" }
         }
         lappend synopsysFlags $flagNparam
         lappend OPTIONS $flagNdesc
         lappend lcol [string first ":" $flagNdesc]
      }

      if { [string length "[lindex [split $SYNOPSYS \n] end] $synopsysFlags "] > 80 } {
         append SYNOPSYS "\n    "
      }
      if { ! $mandatory } {
         append SYNOPSYS " \[[join $synopsysFlags]\]"
      } elseif { [llength $synopsysFlags] == 1 } {
         append SYNOPSYS " [join $synopsysFlags |]"
      } else {
         append SYNOPSYS " \{[join $synopsysFlags |]\}"
      }
   }
   lappend OPTIONS ""
   foreach flag [GetToolsFlags general] {
      set flagNparam "-$flag"
      if {[regexp {^\-} $flag]} {
         set flagNparam "   -$flag"
      }
      catch { append flagNparam " <$InfoArgv(-$flag,param)>" }
      catch { append flagNparam "|$InfoArgv(-$flag,longflag)" }
      set flagNdesc "$flagNparam:"
      catch { append flagNdesc " $InfoArgv(-$flag,desc)" }
      lappend OPTIONS $flagNdesc
      lappend lcol [string first ":" $flagNdesc]
   }

   puts "$SYNOPSYS"
   if {$onlySynopsys} { return }

   # OPTIONS
   regsub -all "<>" $OPTIONS "" newOPTIONS
   set colIndent [lindex [lsort -integer $lcol] end]
   set OPTIONS ""
   foreach option $newOPTIONS {
      set colIndx [string first ":" $option]
      regsub ":" $option "[Bar " " [expr $colIndent - $colIndx]]:" option
      lappend OPTIONS $option
   }
   lappend lcol [string first ":" $option]

   set text [split $helpPage($G(var:tool.name)) \n]
   set index 0
   set read 0
   foreach line $text {
      incr index
      if { [incr read [expr [regexp "DESCRIPTION" $line] + ( $read && ! [regexp {[^ ]} $line] ) ]] > 1 } {
         break;
      }
   }

   puts "\n[join [lrange $text 0 [expr $index -1]] \n]"
   PutsIn80Chars "[join $OPTIONS "\n  "]" -chars ": "
   PutsIn80Chars "\n[join [lrange $text $index end ] \n]" -chars "-"

   return
}
######################################################################

