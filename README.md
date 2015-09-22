# Oracle-XXOBJPY-Project
XXOBJPY Project is a tool to extract DDL scripts form a object list.
This Project is written in those technologies bellow:
* PL/SQL   : the main algorithm to extract object scripts as CLOB.
* Python2  : the main UI to read config fle/object list, generate ourput scripts as zip files.
* cx_oracle: use to connect db and handle sql objects.

## Features

* Maintain object lists, and extract those scripts anytime.
* Support for table/view/trigger/index/package/java source.
* Support for fndload scripts.
* Support to create scripts with config file.
* Group objects with owner, code name, and zip to a file.
