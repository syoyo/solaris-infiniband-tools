### This script is running over ibis (/usr/bin/ibis)
source [file join [file dirname [info script]] ibdebug.tcl]

######################################################################
#  IB Debug Tools
#  NAME
#     ibdiagnet
#
#  COPYRIGHT
#
#     This software is available to you under a choice of one of two
#     licenses.  You may choose to be licensed under the terms of the GNU
#     General Public License (GPL) Version 2, available from the file
#     COPYING in the main directory of this source tree, or the
#     OpenIB.org BSD license below:
#   
#         Redistribution and use in source and binary forms, with or
#         without modification, are permitted provided that the following
#         conditions are met:
#   
#          - Redistributions of source code must retain the above
#            copyright notice, this list of conditions and the following
#            disclaimer.
#   
#          - Redistributions in binary form must reproduce the above
#            copyright notice, this list of conditions and the following
#            disclaimer in the documentation and/or other materials
#            provided with the distribution.
#   
#     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#     EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#     NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
#     BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
#     ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
#     CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#     SOFTWARE.
#
# * Copyright (c) 2005 Mellanox Technologies. All rights reserved.
#
# DATAMODEL
#     Note: all global variables are placed in the array G
#
#  FUNCTION
#     ibdiagnet discovers the entire network. providing text display of the result as well as subnet.lst,
#     LFT dump (same format as osm.fdbs) and Multicast dump (same as osm.mcfdbs).
#     The discovery exhaustively routes through all the fabric links multiple times,
#     tracking and reporting packet drop statistics - indicating bad links if any.
#
#  AUTHOR
#  Danny Zarko. Mellanox Technologies LTD.
#
#  CREATION DATE
#  04/Aug/05
#
#  MODIFICATION HISTORY
#  $Revision: 2608 $
#  Initial Revision.
#
#  NOTES
#
#******
######################################################################

######################################################################
### Action
######################################################################
### Initialize ibis and pre-setting for ibdiag
InitializeIBDIAG
InitializeINFO_LST
StartIBDIAG

set G(bool:bad.links.detected) 1

### Discover the cluster
if {[catch {DiscoverFabric 0} e]} {
   puts <$e:$errorInfo>
   ### Discover the hidden cluster
   if {[catch {DiscoverHiddenFabric} e]} {
      inform "-I-discover:discovery.status"
      inform "-I-exit:\\r"
      inform "-V-discover:end.discovery.header"
      inform "-E-discover:broken.func" $errorInfo $e
   }
}

writeDBFile

### Write the .lst and .mask files
writeMasksFile
writeLstFile

### match topology (if topology was given)
set G(bool:topology.matched) [expr ([MatchTopology $G(outfiles,.lst)] == 0)]
DumpBadLidsGuids
DumpBadLinksLogic
CheckSM
PMCounterQuery
CheckAllinksSettings
CheckPartitions
CheckIPoIB

### Write the .fdbs, .mcfdbs, and .sm files
writeFdbsFile
writeMcfdbsFile
writeSMFile

### output info about bad/broken links
DumpBadLinks

### report the results of topology matching (after bad links report)
DumpTopologyMatching

### Dump out Topology if requested
writeTopologyFileAndIBNLs

### run packages provided procs
catch {RunPkgProcs} e

### report fabric qualities
if {[catch {DumpFabQualities} e]} { puts "\n\nERROR $errorInfo $e" ; exit 1}

### Finishing
FinishIBDIAG
######################################################################
package provide $G(var:tool.name)

