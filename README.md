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


##Requirements and Setup
* Python2.x: </br> 
  https://www.python.org/downloads/ 
* cx_oracle: </br>
  http://cx-oracle.sourceforge.net/
* Oracle Clinet: </br>
  http://www.oracle.com/technetwork/topics/winsoft-085727.html  </br>
  e.g.  instantclient-basic-nt-11.2.0.2.0.zip (http://www.oracle.com/technetwork/topics/winsoft-085727.html),
  Unzip the file, and move it to C:\Python27\instantclient_11_2
* System Varaiables: </br>
  (1). Add System Varaible named ORACLE_HOME, e.g. C:\OracleD10G, this is for locate the TNSNAMES.ORA. </br>
      Or use the command : <code> set ORACLE_HOME=C:\OracleD10G </code></br>
  (2). Add Oracle Clinet which downloaded before to System Variable PATH </br>
      Or use the command : <code> set PATH=C:\Python27\instantclient_11_2;%PATH% </code> </br>
  
##Run the Program
* put xxGenDDLMain.py and xxobj.py in the same folder and then execute xxGenDDLMain.py </br>
  <code> python xxGenDDLMain.py </code>

