#!/bin/sh

# Call solaris_set_nodedesc, if operating system is SunOS
os=`uname -s`
if [ $os == "SunOS" ]; then
	solaris_set_nodedesc
	rc=$?
	exit $rc
fi

# set the node_desc field of any hca found to the defined hostname

. /etc/sysconfig/network

ib_sysfs="/sys/class/infiniband"

for hca in `ls $ib_sysfs`; do
   if [ -f $ib_sysfs/$hca/node_desc ]; then
      echo -n "$HOSTNAME" >> $ib_sysfs/$hca/node_desc
   else
      logger -s "Failed to set node_desc for : $hca"
   fi
done

exit 0
