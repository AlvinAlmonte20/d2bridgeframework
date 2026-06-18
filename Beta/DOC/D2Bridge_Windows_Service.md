# ⚙️ D2Bridge Framework

## How to Run the EXE as a Windows Service

> **Author of findings:** Core analysis of the D2Bridge Framework (Beta)
>
> **Date:** April 2026

---

# 📑 Índice

1. [📋Overview](#1-overview)

2. [🔍How the Framework Detects Service Mode](#2-how-the-framework-detects-service-mode)

3. [🔄Changes in Server Behavior](#3-changes-in-server-behavior)

4. [📂Configuring Server Port and Name Without a Console](#4-configuring-server-port-and-name-without-a-console)

5. [💻Registering the Application as a Windows Service](#5-registering-the-application-as-a-windows-service)

6. [🚀Full Service Initialization Flow](#6-full-service-initialization-flow)

7. [⚠️Important Notes](#7-%EF%B8%8Fimportant-notes)

---

### 1. 📋Overview

* The D2Bridge Framework core already includes native support for detection and execution as a Windows Service.

* No changes to the project source code or the `.dpr` file are required.

* The framework automatically detects when it is running as a Windows service and adapts its behavior accordingly.

---

### 2. 🔍How the Framework Detects Service Mode

In the file `D2Bridge.Util.pas`, the function `IsRunningAsService` determines whether the application is running as a Windows service by combining two conditions:

1. The process is running in **Windows Session 0**, which is reserved exclusively for system services.
2. There is **no interactive desktop** available, meaning the call to `OpenInputDesktop` returns an invalid handle.

If both conditions are true, the function returns `TRUE`, indicating that the process is executing as a Windows service.

#### Code excerpt (`D2Bridge.Util.pas`):
```pascal
function IsRunningAsService: Boolean;
var
  SessionId: DWORD;
  hDesk: THandle;
begin
  Result := False;
  if not ProcessIdToSessionId(GetCurrentProcessId, SessionId) then
    Exit(False);
    
  if SessionId <> 0 then
    Exit(False);
    
  hDesk := OpenInputDesktop(0, False, DESKTOP_SWITCHDESKTOP);
  if hDesk <> 0 then
  begin
    CloseDesktop(hDesk);
    Exit(False);
  end;
  
  Result := True; // Session 0 + no interactive desktop = service
end;
```
---

### 3. 🔄Changes in Server Behavior

In `D2Bridge.ServerControllerBase.pas`, the method `CheckNeedConsole` uses the result returned by `IsRunningAsService`.
```pascal
function TD2BridgeServerControllerBase.CheckNeedConsole: boolean;
begin
  result := true;
  {$IFDEF MSWINDOWS}
  result := not IsRunningAsService;
  {$ENDIF}
  result := not IsD2DockerContext;
end;
```
When `IsRunningAsService` returns `TRUE`, the `NeedConsole` flag becomes `FALSE`. As a result, all console-related interaction is automatically skipped.

Specifically:

* The interactive prompt for server port and server name is **not displayed**.

* The status screen with counters and resource monitoring is **not displayed**.

* The server **starts immediately** without any user interaction.

* The internal main loop continues to run normally, but without console output, performing **internal monitoring only**.

---

### 4. 📂Configuring Server Port and Name Without a Console

When running without a console, the server reads configuration values directly from a file named `Config.ini`, located in the same directory as the executable file.

#### Code excerpt (`D2Bridge.APPConfig.pas`):
```pascal
function TD2BridgeAPPConfig.ServerPort(ADefaultPort: Integer): Integer;
begin
  Result := FFileINIConfig.ReadInteger('D2Bridge Server Config', 'Server Port', ADefaultPort);
end;

function TD2BridgeAPPConfig.ServerName(...): String;
begin
  Result := FFileINIConfig.ReadString('D2Bridge Server Config', 'Server Name', ADefaultServerName);
end;
```
#### Required configuration file

Before registering the application as a service, create a file named `Config.ini` in the same directory as the executable with the following content:
```ini
[D2Bridge Server Config]
Server Port=8888
Server Name=YourServerName
```
> 💡 Replace `8888` with your desired port number and `YourServerName` with your preferred server name.

---

### 5. 💻Registering the Application as a Windows Service

> 🚨 **Prerequisite:** Run Command Prompt (`cmd`) or PowerShell as **Administrator**.

#### Option A: Using `SC.EXE` (Native Windows Tool)

* **Register the service:**
  ```bash
  sc create ServiceName binPath= "C:\Full\Path\Application.exe" start= auto DisplayName= "Visible Service Name"
  ```
* **Start the service:**
  ```bash
  sc start ServiceName
  ```
* **Stop the service:**
  ```bash
  sc stop ServiceName
  ```
* **Remove the service:**
  ```bash
  sc delete ServiceName
  ```
> ⚠️ **Important:** The space after `binPath=` and `start=` is strictly mandatory when using `sc.exe`.

#### Option B: Using NSSM (Recommended for Production)

NSSM (Non-Sucking Service Manager) is a free tool that provides automatic restart on crashes and supports redirection of output to log files.

* **Download:** <https://nssm.cc/download>

* **Install using the graphical interface:**
  ```bash
  nssm install ServiceName
  ```
* **Install via command line:**
  ```bash
  nssm install ServiceName "C:\Full\Path\Application.exe"
  ```
* **Start the service:**
  ```bash
  nssm start ServiceName
  ```
* **Stop and remove the service:**
  ```bash
  nssm stop ServiceName
  nssm remove ServiceName confirm
  ```
---

### 6. 🚀Full Service Initialization Flow

1. Windows starts the service in Session 0 without an interactive desktop.

2. The executable process is launched.

3. `IsRunningAsService` returns `TRUE`.

4. `NeedConsole` is set to `FALSE`.

5. `TD2BridgeServerConsole.Run` is executed (same logic as console mode).

6. The console configuration screen is skipped.

7. Server port and server name are read from `Config.ini`.

8. `D2BridgeServerController.StartServer` is executed normally.

9. The HTTP/HTTPS server starts accepting connections.

10. The internal loop keeps the process alive while the server is running.
---
### 7. ⚠️Important Notes

* The `.dpr` file **does not require any modification**.

* The `Config.ini` file **must exist** before starting the service for the first time. If it does not exist, default values defined in the code will be used (port 8888 and the default server name).

* **SSL certificate paths** must be configured in the code before compilation, as there is no interactive way to configure them when running as a service.

* Errors and crashes can be inspected using **Windows Event Viewer**: `eventvwr.msc` $\rightarrow$ _Windows Logs_ $\rightarrow$ _Application_.

* The service runs under the **SYSTEM account** by default. If access to network resources is required (remote databases, shared folders, etc.), configure a dedicated service account with appropriate permissions.
