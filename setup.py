########################################################################
### setup.py
###

import os
from distutils.core import setup, Extension

setup(name="soft430",
      version="0.1",
      author="mark hays",
      description="msp430f1611 core simulator",
      py_modules=["py430"],
      ext_modules=[
        Extension("_soft430",
                  ["soft430.c"],
                  define_macros=[("WITH_PYTHON", None)],
                  include_dirs=["."],
                  library_dirs=["."],
                  libraries=[]),
        Extension("_py430",
                  ["py430.c"],
                  define_macros=[ ],
                  include_dirs=["."],
                  library_dirs=["."],
                  libraries=[]),
      ])

### EOF setup.py

