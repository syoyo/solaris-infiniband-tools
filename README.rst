Solaris InfiniBand tools
========================

USE YOUR OWN RISK!!!

License
-------

CDDL.

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

