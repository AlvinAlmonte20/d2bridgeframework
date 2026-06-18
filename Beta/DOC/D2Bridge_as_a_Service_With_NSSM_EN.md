# 🌐 Installing D2Bridge Delphi applications as a Windows service with NSSM

## 📑 Table of Contents

* [Basic Information](#-basic-information)
    * [❓¿What is NSSM?](#what-is-nssm)
    * [Advantages of NSSM](#advantages-of-nssm)
* [Installation and Configuration](#-installation-and-configuration)
    * [Installing NSSM](#installing-nssm)
    * [Verifying if NSSM is installed correctly](#verifying-if-nssm-is-installed-correctly)
    * [Run D2Bridge Web App](#run-d2bridge-web-app)
---

## 📋 Basic Information

### ❓¿What is NSSM?

NSSM, or Non-Sucking Service Manager, is an open-source utility for managing services in Windows operating systems. It allows you to run any application as a Windows service, offering features such as automatic restart if the service fails, and includes a graphical service installer and uninstaller. NSSM is compatible with Windows 2000 and later versions, including Windows 7, 8, and 10. It simplifies service management compared to the basic Windows Services utility, offering additional options for better control.

### Advantages of NSSM

* **Non-Sucking Service Manager (NSSM)** is a free tool that allows you to convert almost any application or executable, including console applications, into a Windows service.

* It is responsible for **monitoring and restarting** the application if it stops unexpectedly.

* Log events in the **Event Viewer** for diagnostics.

* It offers a simple graphical interface for configuring parameters such as working directory, arguments, and automatic recovery.

* Backward compatibility with previous versions of Windows (Windows Server 2008 R2/Windows 7 and earlier, although not tested).

* Ideal for Windows Server

* Automatic startup on boot without login (The user who created the startup entry does not need to log in for it to start).

* Run both binaries as Services.

* Independent (no dependency on Node.js)

---

## 🚀 Installation and Configuration

### Installing NSSM

Please download and extract [NSSM](https://github.com/dkxce/NSSM/releases/download/v2.25/NSSM_v2.25.zip), selecting the appropriate architecture for your Windows system (if x86, use the contents of the win32 folder; if x64, use the contents of the win64 folder). It is also good practice to move the NSSM binary to the `Program Files\NSSM` directory (NSSM, once started as a service, cannot be moved from its original directory; therefore, it is best to save it in Program Files) on your installation drive (usually the `C:` drive). It is also recommended to add the path (such as `C:\Program Files\NSSM`) to the `PATH environment variable`.

### Verifying if NSSM is installed correctly

If you have done everything correctly, the folder I'm using the `C:\Program Files\NSSM` (in this example `C:` drive, but you can use any drive where you installed Windows or any path you prefer) should only contain the **nssm.exe** file.

We will use `C:\Program Files\NSSM` in this example.

Open the Command Prompt and run `nssm`. If you see a help page, you are ready to proceed to the next step.

### Run D2Bridge Web App

Copy the D2Bridge application to `D:\RCSystem\WebApp\WebAppCoopABC.exe` (or anywhere you like, just make sure it doesn't change after installing the service). Now return to the command prompt. 

We will use `D:\RCSystem\WebApp\WebAppCoopABC.exe` in this example. 
```Bash
nssm install “webappcoopabc” “D:\RCSystem\WebApp\WebAppCoopABC.exe”
```
### Note:

> - You can change the service name to WebAppCoopabc.
>
> - You can change D:\RCSystem\WebApp\WebAppCoopABC.exe to the location where
>
> - you placed the D2Bridge application

### Command templates:

The command template in case you only want to copy, paste, and edit.

#### Installing service
```Bash
nssm install <Desired D2Bridge Service Name> <WebApp Route>
```
#### Start services
After successful installation of the services, they must be started.
```Bash
nssm start <D2Bridge Service name>
```
Ready!