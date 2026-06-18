# 📊D2Bridge Resource Monitor

Usage Guide

## 📑 Table of Contents

* [❓What Is the Resource Monitor](#What-Is-the-Resource-Monitor)
* [🔍What Is Collected in Each Snapshot](#What-Is-Collected-in-Each-Snapshot)
* [⚙️Configuration Parameters](#Configuration-Parameters)
* [📂Where the Log Is Written](#Where-the-Log-Is-Written)
    * [Example Log Entry](#Example-Log-Entry)
* [📝Configuration - Option 1: Using an INI File (Recommended)](#Configuration-Option-1:-Using-an-INI-File-(Recommended))
    * [Ini File Example](#Ini-File-Example)
* [💻Configuration - Option 2: Programmatic Configuration (Properties)](#Configuration-Option-2:-Programmatic-Configuration-(Properties))
* [⚠️Important Notes](#Important-Notes)
---

### ❓What Is the Resource Monitor

The Resource Monitor is a diagnostic telemetry module of D2Bridge that periodically captures the resource state of a Windows server and records this information in the application log file.

* It operates independently from functional logs such as exceptions, access, and security logs.

* Its primary purpose is to help identify CPU and memory bottlenecks in production environments.

### 🔍What Is Collected in Each Snapshot

Each snapshot records the following information:

* **Level:** Indicates Info or Warning. A Warning level is triggered when CPU or memory usage exceeds the configured thresholds.

* **MachineName:** Windows server name.

* **App / PID:** Application executable name and current process ID.

* **AppCPU:** Percentage of CPU consumed by the application process since the last interval.

* **AppWorkingSetMB:** Physical memory allocated to the process (Working Set).

* **AppCommitMB:** Virtual memory committed by the process.

* **SystemMemoryLoad:** Percentage of total physical memory usage by Windows.

* **SystemAvailable MemoryMB:** Available physical memory in the system.

* **System TotalPhysicalMemoryMB:** Total installed RAM.

* **Sessions:** Number of active sessions on the D2Bridge server.

* **TopCPU:** The top N processes with the highest CPU usage at the time of the snapshot.

* **TopMemory:** The top N processes with the highest Working Set at the time of the snapshot.

> **Note:** During the first collection after server startup, `AppCPU` and `TopCPU` appear as _collecting_, because no previous interval exists for comparison.

### ⚙️Configuration Parameters

The following parameters control the behavior of the Resource Monitor:

* **Resource Monitor**

  * **Type:** Boolean
  * **Default:** False
  * _Description:_ Enables or disables the resource monitor.

* **Resource MonitorIntervalSec**

  * **Type:** Integer
  * **Default:** 300
  * _Description:_ Interval between snapshots, in seconds (minimum value: 15).

* **Resource Monitor Top ProcessCount**

  * **Type:** Integer
  * **Default:** 3
  * _Description:_ Number of processes listed in Top CPU and Top Memory.

* **Resource MonitorCPUAlertPercent**

  * **Type:** Integer
  * **Default:** 70
  * _Description:_ Application CPU usage percentage that elevates the log level to Warning.

* **Resource Monitor MemoryAlertMB**

  * **Type:** UInt64
  * **Default:** 2048
  * _Description:_ Application Working Set (in MB) that elevates the log level to Warning.

* **To disable CPU alerts**, set `Resource Monitor CPUAlertPercent` to 0.

* **To disable memory alerts**, set `Resource Monitor MemoryAlertMB` to 0.

### 📂Where the Log Is Written

Each snapshot is written using the `LogDiagnostic` method to the same log file used by the application, located in:

Plaintext

```
wwwroot\log\
```

The log file behavior follows the `LogFileMode` setting:

* **session:** One log file per server execution (default).

* **daily:** One log file per day, using the format `YYYY_MM_DD.txt`.

The log object is created automatically when the server starts with `Resource Monitor = True`, even if other logs (exception, access, security) are disabled.

#### Example Log Entry

Each snapshot is recorded as a single diagnostic log entry containing all collected fields.

> (The exact format depends on the configured log layout.)

### 📝Configuration - Option 1: Using an INI File (Recommended)

This is the recommended approach for production environments, as it allows configuration changes without recompiling the application.

The project INI file must contain a `[D2Bridge Log]` section with the parameters listed above. If a parameter does not exist, D2Bridge automatically creates it with the default value during the first execution.

To enable INI-based configuration, the `UseINIConfig` option must be enabled. 

Verify that the property is enabled in the `ServerController` component, or enable it by code before calling `StartServer`.

#### Ini File Example:

Ini, TOML
```ini
[D2Bridge Log]
LogFileMode=daily
Resource Monitor=True
Resource MonitorIntervalSec=20
Resource Monitor Top ProcessCount=3
Resource Monitor CPUAlertPercent=70
Resource Monitor MemoryAlertMB=2048
```
### 💻Configuration - Option 2: Programmatic Configuration (Properties)

This approach is useful when you do not want to rely on an INI file, or when configuration must be performed dynamically at runtime before `StartServer`.

Internally, the `TPrism Resource Monitor.Configure` method is invoked. When configuring by code, the correct approach is:

1. Disable `UseINIConfig`.

2. Call the configuration method during the `OnBeforeServerStart` event (or in the `WebApp FormCreate`, before calling `StartServer`).

> **Important:** Verify the exact method name exposed by your `ServerController`. In some projects it may be accessible via `Prism.Resource Monitor.Configure(...)` or through a wrapper in `ServerControllerBase`.
> 
> Use IntelliSense with `ServerController.Prism.` to locate the correct member.

### ⚠️Important Notes

> * The Resource Monitor **operates only on Windows**, as it relies on Windows APIs such as `psapi.dll`, `kernel32.dll`, and `GetSystemTimes`. On Linux or Lazarus targets, the module is compiled but does not collect data.
>
> * The Resource Monitor timer is **independent from the session timer**. Enabling or disabling the monitor does not affect session heartbeats.
>
> * An **initial snapshot is generated immediately** after the server finishes startup (`StartServer`), before the first timer interval, providing an immediate baseline in the log.
>
> * The `ResourceMonitor` field in the `[D2Bridge Log]` section of the INI file must be set to `True`. The INI reader uses `ReadBool`, so the value is **case-insensitive**.
