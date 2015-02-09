from distutils.core import setup
import py2exe,sys

sys.argv.append('py2exe')

setup(
    options={"py2exe" : {"dll_excludes": ["OCI.dll",], "includes" : ["decimal", ]}},
    console = [{'script': "xxGenDDLMain.py"}],
    zipfile = None
    )