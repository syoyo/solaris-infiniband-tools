proc ibis_load1.2 {dir} {
   puts "Loading package ibis from: $dir"
   uplevel \#0 load [file join $dir libibis.so.1.2]
}

package ifneeded ibis 1.2 [list ibis_load1.2 $dir]
