
proc ibdiagnet-load {dir} {
   puts "Loading IBDIAGNET from: $dir"
   uplevel #0 source [file join $dir ibdiagnet.tcl]
}
package ifneeded ibdiagnet 1.0 [list ibdiagnet-load $dir]
