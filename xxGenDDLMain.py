from Tkinter import *
from tkFileDialog import askopenfilename, askdirectory
from xxobj import XXOBJ


class genDDLGui(Frame):
    def __init__(self, master=None):
        Frame.__init__(self, master)
        self.grid()
        self.createWidgets()

    def createWidgets(self):
        self.connStr = Label(self)
        self.connStr["text"] = "Connect String:"
        self.connStr.grid(row=0, column=0)
        self.connStr = Entry(self)
        self.connStr["width"] = 100
        self.connStr.grid(row=0, column=1, columnspan=5)

        self.strDef = Button(self)
        self.strDef["text"] = "Def"
        self.strDef.grid(row=0, column=6)
        self.strDef["command"] = self.strDefMethod

        self.cfgFile = Label(self)
        self.cfgFile["text"] = "Config File:"
        self.cfgFile.grid(row=1, column=0)
        self.cfgFile = Entry(self)
        self.cfgFile["width"] = 100
        self.cfgFile.grid(row=1, column=1, columnspan=5)

        self.cfgSel = Button(self)
        self.cfgSel["text"] = "Select"
        self.cfgSel.grid(row=1, column=6)
        self.cfgSel["command"] = self.cfgSelMethod

        self.objList = Label(self)
        self.objList["text"] = "Object List:"
        self.objList.grid(row=2, column=0)
        self.objList = Entry(self)
        self.objList["width"] = 100
        self.objList.grid(row=2, column=1, columnspan=5)

        self.objSel = Button(self)
        self.objSel["text"] = "Select"
        self.objSel.grid(row=2, column=6)
        self.objSel["command"] = self.objSelMethod

        self.outPath = Label(self)
        self.outPath["text"] = "Output Path:"
        self.outPath.grid(row=3, column=0)
        self.outPath = Entry(self)
        self.outPath["width"] = 100
        self.outPath.grid(row=3, column=1, columnspan=5)

        self.outSet = Button(self)
        self.outSet["text"] = "Set"
        self.outSet.grid(row=3, column=6)
        self.outSet["command"] = self.outSetMethod

        self.btnGenScript = Button(self)
        self.btnGenScript["text"] = "Gen Script"
        self.btnGenScript.grid(row=4, column=1)
        self.btnGenScript["command"] = self.btnGenScriptMethod

        self.btnPurgeTemp = Button(self)
        self.btnPurgeTemp["text"] = "Purge Temp"
        self.btnPurgeTemp.grid(row=4, column=2)
        self.btnPurgeTemp["command"] = self.btnPurgeTempMethod

    def strDefMethod(self):
        self.connStr.delete(0, END)
        self.connStr.insert(0, "apps/v05appS126@asus_ebs_8005")
        self.cfgFile.delete(0, END)
        self.cfgFile.insert(0, 'C:/SETUP.cfg')
        self.objList.delete(0, END)
        self.objList.insert(0, 'C:/XXARF0504.txt')
        self.outPath.delete(0, END)
        self.outPath.insert(0, 'C:/')


    def cfgSelMethod(self):
        Tk().withdraw()
        filename = askopenfilename()
        self.cfgFile.delete(0, END)
        self.cfgFile.insert(0, filename)

    def objSelMethod(self):
        Tk().withdraw()
        filename = askopenfilename()
        self.objList.delete(0, END)
        self.objList.insert(0, filename)

    def outSetMethod(self):
        Tk().withdraw()
        dirname = askdirectory()
        self.outPath.delete(0, END)
        self.outPath.insert(0, dirname)

    def btnGenScriptMethod(self):
        with XXOBJ(self.connStr.get()) as theobj:
            theobj.getxxid()
            print theobj.xxid
            theobj.readcfgfile(self.cfgFile.get())
            theobj.readobjlist(self.objList.get())
            theobj.initobjlistinfo()
            theobj.genobjddl(self.outPath.get())

    def btnPurgeTempMethod(self):
        with XXOBJ(self.connStr.get()) as theobj:
            theobj.purgeoldtemp()


if __name__ == '__main__':
    root = Tk()
    app = genDDLGui(master=root)
    app.mainloop()

