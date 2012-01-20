Solaris InfiniBand tools
========================

USE YOUR OWN RISK!!!

License
-------

CDDL or OFED license.

Prebuild packages for OpenIndiana 151a
--------------------------------------

SUNWofusr
~~~~~~~~~~~~~~~~

Precompiled package of ofusr(OFED userland with Solaris patch).
Note that OpenSM binary is not included in this package.

Install SUNWofusr package by

::

  $ sudo pkgadd -d oi151a/sfw/packages/i386/nightly-nd/SUNWofusr

Test.

::

  $ ib_rdma_bw                   # On solaris, as a server
  $ rdma_bw solaris_ipaddr       # On Linux, as a client


Patched Tavor driver for Illumos(OI151a) kernel
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A compiled **tavor** driver(InfiniHost III) with my patch ( https://github.com/syoyo/illumos-gate/tree/tavor )
This patched tavor driver enables running OpenSM on OpenIndiana.

Located at ::

  oi151a/kernel/drv

