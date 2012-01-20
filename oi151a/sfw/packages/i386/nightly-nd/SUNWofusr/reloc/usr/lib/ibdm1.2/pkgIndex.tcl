# Main idea here is to intialize the ibnl_path
proc ibdmLoad1.2 {dir} { 
   global ibnl_path 
   global env
   # support env variable for extending the search path for
   # system definition files.
   if {[info exists env(IBADM_IBNL_PATH)]} {
      set ibnl_path "$env(IBADM_IBNL_PATH)"
   } else {
      set ibnl_path ""
   }
   puts "Loading IBDM from: $dir"
   uplevel \#0 load [file join $dir libibdm.so.1.2]
	package provide ibdm 1.2
}

package ifneeded ibdm 1.2 [list ibdmLoad1.2 $dir]
