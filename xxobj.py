import cx_Oracle
import csv
import os
import zipfile
import shutil


def make_zipfile(output_filename, source_dir):
    relroot = os.path.abspath(os.path.join(source_dir, os.pardir))
    with zipfile.ZipFile(output_filename, "w", zipfile.ZIP_DEFLATED) as zip:
        for root, dirs, files in os.walk(source_dir):
            # add directory (needed for empty dirs)
            zip.write(root, os.path.relpath(root, relroot))
            for file in files:
                filename = os.path.join(root, file)
                if os.path.isfile(filename):  # regular files only
                    arcname = os.path.join(os.path.relpath(root, relroot), file)
                    zip.write(filename, arcname)

class XXOBJ:
    def __init__(self, connStr):
        self.db = cx_Oracle.Connection(connStr)
        self.cursor = self.db.cursor()
        self.xxid = 0

    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        self.db.commit()
        self.cursor.close()
        self.db.close()

    def testprc(self):
        clob = self.cursor.callfunc("xx_obj_py_pkg.get_pkg_clob", cx_Oracle.CLOB, )
        print type(clob)

    def getxxid(self):
        self.xxid = self.cursor.callfunc("xx_obj_py_pkg.GET_XX_OBJ_ID", int, )

    def readcfgfile(self, cfgfile):
        with open(cfgfile) as f1:
            for row in csv.reader(f1):
                if row[1] == 'SCHEMA_INFO':
                    print "call schema info prc " + row[2]
                    self.cursor.callproc("xx_obj_py_pkg.REC_SCHEMA_INFO", [self.xxid, row[2], row[3], row[4], row[5]])
                else:
                    self.cursor.callproc("xx_obj_py_pkg.REC_CFG_PARAM", [self.xxid, row[1], row[2]])

    def readobjlist(self, objlist):
        with open(objlist) as f1:
            for row in csv.reader(f1):
                if len(row) == 5:
                    self.cursor.callproc("xx_obj_py_pkg.REC_CUST_OBJ",
                                         [self.xxid, row[0], row[1], row[2], row[3], row[4], None])
                elif len(row) == 3:
                    self.cursor.callproc("xx_obj_py_pkg.REC_CUST_OBJ",
                                         [self.xxid, row[0], row[1], row[2], None, None, None])
                elif len(row) == 4:
                    self.cursor.callproc("xx_obj_py_pkg.REC_CUST_OBJ",
                                         [self.xxid, row[0], row[1], row[2], row[3], None, None])
                else:
                    self.cursor.callproc("xx_obj_py_pkg.REC_CUST_OBJ",
                                         [self.xxid, row[0], row[1], row[2], row[3], row[4], row[5]])

    def initobjlistinfo(self):
        self.cursor.callproc("xx_obj_py_pkg.GEN_OBJ_DDL_LIST", [self.xxid])

    def purgeoldtemp(self):
        self.cursor.callproc("xx_obj_py_pkg.PURGE_TEMP_TABLES")

    def genobjddl(self, outPath):
        query = "SELECT PROGRAM_CODE||'/'||OWNER PATH,OBJECT_TYPE,OBJECT_NAME,SCRIPT_FILE_NAME FROM XX_DDL_SCRIPT_LIST_TMP WHERE xx_cust_id = :xx_id"
        self.cursor.execute(query, xx_id=self.xxid)
        rows = self.cursor.fetchall()
        for vpath, vobjType, vobjName, vfileName in rows:
            path = outPath + vpath
            if not os.path.exists(path):
                os.makedirs(path)
            clob = self.cursor.callfunc("xx_obj_py_pkg.get_obj_ddl", cx_Oracle.CLOB, [vobjType, vobjName])
            with open(path+vfileName, 'w') as fc:
                fc.write(clob.read())
                fc.close()

        query = "SELECT PROGRAM_CODE,COUNT(1) CNT FROM XX_DDL_SCRIPT_LIST_TMP WHERE xx_cust_id = :xx_id GROUP BY PROGRAM_CODE"
        self.cursor.execute(query, xx_id=self.xxid)
        rows = self.cursor.fetchall()
        for vpCode,vcnt in rows:
            path = outPath + vpCode
            zfile = outPath + vpCode+".zip"
            make_zipfile(zfile, path)
            shutil.rmtree(path)

        print "Done!"