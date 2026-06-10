# 🌐 D2Bridge Framework - Unofficial Encyclopedia

<div align="center">

![D2Bridge Logo](/assets/LogoD2BridgeTransp.png)

[![License: LGPL 2.1](https://img.shields.io/badge/License-LGPL%202.1-blue.svg)](https://opensource.org/licenses/LGPL-2.1)
[![Delphi](https://img.shields.io/badge/Delphi-10.0--13.0-orange.svg)](https://www.embarcadero.com/products/delphi)
[![Lazarus](https://img.shields.io/badge/Lazarus-3.4--4.4-green.svg)](https://www.lazarus-ide.org/)

[GitHub](https://github.com/d2bridge/d2bridgeframework) • [Discord](https://discord.gg/WvHaWP6h9t) • [Website](https://d2bridge.com.br)

</div>

---

## 📑 Table of Contents

- [Basic Information](#-basic-information)
- [Installation and Configuration](#-installation-and-configuration)
- [Framework Architecture](#-framework-architecture)
- [Translation System](#-translation-system)
- [Callbacks and Events](#-callbacks-and-events)
- [Templates](#-templates)
- [Components](#-components)
- [Forms](#-forms)
- [CRUD System](#-crud-system)
- [Database](#-database)
- [Email API](#-email-api)
- [Token System](#-token-system)
- [Popup and Nested Forms](#-popup-and-nested-forms)
- [File Upload](#-file-upload)
- [Validation](#-validation)
- [Sessions and Security](#-sessions-and-security)
- [Application Lifecycle](#-application-lifecycle)
- [Best Practices](#-best-practices)
- [Troubleshooting](#-troubleshooting)
- [Sample Project](#-sample-project)

---

## 📋 Basic Information

### What is D2Bridge?

D2Bridge is an open-source framework that enables converting Delphi or Lazarus applications into web applications **without writing any JavaScript code**. The framework maintains the native Delphi approach to programming while generating modern applications that run in the browser.

### Technical Data

| Parameter | Value |
|-----------|-------|
| **Author** | Talis Jonatas Gomes |
| **License** | LGPL 2.1 |
| **Beta Version** | 2.5.83 |
| **Stable Version** | 2.0.8 |
| **Delphi** | 10.0 - 13.0 |
| **Lazarus** | 3.4 - 4.4 (Windows) |
| **Email** | talisjonatas@me.com |

### Links

| Resource | URL |
|----------|-----|
| **GitHub Repository** | https://github.com/d2bridge/d2bridgeframework |
| **Discord Server** | https://discord.gg/WvHaWP6h9t |
| **Official Website** | https://d2bridge.com.br |

### Supported Languages (Translations)

D2Bridge natively supports 18 languages:

| Code | Language |
|------|----------|
| `AR_SA` | Arabic (Saudi Arabia) |
| `CS_CZ` | Czech |
| `DE_DE` | German |
| `EN_US` | English (USA) |
| `ES_ES` | Spanish |
| `FA_IR` | Persian (Iran) |
| `FR_FR` | French |
| `IT_IT` | Italian |
| `JA_JP` | Japanese |
| `KO_KR` | Korean |
| `PL_PL` | Polish |
| `PT_BR` | Portuguese (Brazil) |
| `RO_RO` | Romanian |
| `RU_RU` | Russian |
| `TH_TH` | Thai |
| `TR_TR` | Turkish |
| `UK_UA` | Ukrainian |
| `ZH_CN` | Chinese (Simplified) |

---

## 🚀 Installation and Configuration

### Creating a New Project

```
File → New → Other → D2Bridge Framework → D2Bridge Framework Delphi Project
```

### Project Parameters

| Parameter | Example | Description |
|-----------|---------|-------------|
| **Name** | `D2Checkin` | Project name |
| **Port** | `8888` | HTTP server port |
| **Platform** | `Web+VCL` | `Web`, `Web+VCL`, `Web+LCL`, `Web+FMX` |
| **Type** | `Server Console` | Application type |

### Directory Structure

```
Project/
├── wwwroot/                 ← Public files (HTML, CSS, JS)
│   ├── css/
│   ├── js/
│   │   └── prismpage.js     ← PrismPage Plugin
│   ├── mail/                ← Email templates
│   └── temp/                ← Session temporary files
├── language/                ← JSON translation files
│   ├── PT_BR.json
│   └── EN_US.json
├── langue_export/           ← Translation export (DEBUG)
├── files/                   ← Application files (OUTSIDE wwwroot!)
│   └── images/
└── Units/
    ├── D2Checkin_Session.pas     ← Project class
    ├── Unit_DM.pas               ← DataModule
    ├── Unit_ServerController.pas ← Server controller
    └── [forms...]
```

### Database Connection Configuration (Config.ini)

```ini
[Database]
Host=localhost
Database=D2Checkin
User=SA
Password=YourPassword
```

---

## 🏗 Framework Architecture

### Threading Model

```
┌─────────────────────────────────────────────────────────────┐
│                    THREADING ARCHITECTURE                   │
├─────────────────────────────────────────────────────────────┤
│  [HTTP Request]                                             │
│        │                                                    │
│        ▼                                                    │
│  ┌─────────────────────┐                                    │
│  │    MAIN THREAD      │  ← Form creation                   │
│  │  (visual instance)  │    (Windows API requirement)       │
│  └─────────────────────┘                                    │
│        │                                                    │
│        ▼                                                    │
│  ┌─────────────────────┐                                    │
│  │   MULTI-THREAD      │  ← Event handling                  │
│  │   (parallelism)     │    Each button = separate thread   │
│  └─────────────────────┘                                    │
└─────────────────────────────────────────────────────────────┘
```

> **Key:** Session ≠ Thread. Objects are bound to `TPrismSession`, not to thread.

### Project Class (Session)

```pascal
type
  TD2Checkin = class(TPrismSessionBase)
  public
    const FILE_PATH = 'files\';
    const FILE_PATH_IMAGES = 'files\images\';
    const DEFAULT_USER_IMAGE = 'files\images\default.jpeg';
    
    // User session variables
    UserID: Integer;
    UserName: string;
    UserEmail: string;
    UserPassword: string;
    UserFotoPath: string;
    UserFoto: string;      // Base64
    AccountID: Integer;
    IsLogged: Boolean;
    AutoJoinEventID: Integer;
  end;
```

### Accessing Project Class

```pascal
// Global access function
function D2Checkin: TD2Checkin;
begin
  Result := TD2Checkin(D2BridgeInstance.PrismSession.Data);
end;
```

---

## 🌍 Translation System

### HTML Tag Syntax

| Type | Syntax | Example | Usage |
|------|--------|---------|-------|
| **Translation** | `{{_name_}}` | `{{_menu_dashboard_}}` | Multilingual text |
| **Variable** | `{{name}}` | `{{username}}` | Dynamic data |
| **System tag** | `{{language}}` | - | Current language |

### Term Definition

```pascal
// D2Bridge_Lingua_App_Term.pas
type
  TD2BridgeLinguaAppTerm = record
    // Menu
    menu_dashboard: string;
    menu_users: string;
    menu_logout: string;
    
    // Buttons
    btn_save: string;
    btn_cancel: string;
    
    // Messages
    MSG_SaveSuccess: string;
    MSG_InvalidEmail: string;
  end;
```

### JSON Files

```json
{
  "menu_dashboard": "Dashboard",
  "menu_users": "Users",
  "menu_logout": "Logout",
  "btn_save": "Save",
  "btn_cancel": "Cancel",
  "MSG_SaveSuccess": "Record saved successfully",
  "MSG_InvalidEmail": "Invalid email address"
}
```

### Translation Generation Process

```
1. Add {{_tag_}} tags in HTML
2. Define in D2Bridge_Lingua_App_Term.pas
3. ⚠️ COMPILE IN DEBUG MODE (generates JSON)
4. Copy JSON from langue_export/ to language/
5. Edit translations - works at RUNTIME!
```

### Usage in Code

```pascal
// Accessing translations
ShowMessage(D2Bridge.LinguaApp.MSG_SaveSuccess);

// In export
.FormGroup(D2Bridge.LApp.user_name, CSSClass.Col.ColMD6)
```

---

## 🔗 Callbacks and Events

### Basic Callbacks (HTML)

```html
<!-- Simple callback -->
<a onclick="{{callback=open_dashboard}}">Dashboard</a>

<!-- Callback with parameter -->
<button onclick="{{callback=open_new_account(free)}}">Free</button>

<!-- Callback with multiple parameters -->
<button onclick="{{callback=action(param1=val1&param2=val2)}}">Action</button>
```

### Handling in Delphi

```pascal
procedure TAdminPageTemplate.Callback(const ACallbackName: string;
  AEventParsing: ID2BridgeEventParsing);
begin
  // Simple callback
  if SameText(ACallbackName, 'open_dashboard') then
  begin
    if FormDashboard = nil then
      FormDashboard := TFormDashboard.Create(nil);
    FormDashboard.Show;
  end
  
  // Callback with parameter
  else if SameText(ACallbackName, 'open_new_account') then
  begin
    SubscriptionType := AEventParsing.Values['subscription'];
    // or first parameter without name:
    SubscriptionType := AEventParsing.Values[0];
  end;
end;
```

### Closing Session

```pascal
// Close callback
if SameText(ACallbackName, 'close_session') then
  Session.Close;
  // or Session.Close(false/true);
```

---

## 🎨 Templates

### Master/Page Template Architecture

```
┌─────────────────────────────────────────┐
│           MASTER TEMPLATE               │
│  ┌─────────┐  ┌──────────────────────┐  │
│  │ SIDEBAR │  │    PAGE TEMPLATE     │  │
│  │  MENU   │  │   ($prismpage)       │  │
│  │         │  │   ┌──────────────┐   │  │
│  │         │  │   │ PRISM BODY   │   │  │
│  │         │  │   │ ($prismbody) │   │  │
│  └─────────┘  └──────────────────────┘  │
└─────────────────────────────────────────┘
```

### PrismPage Plugin

```bash
# Copy plugin
D2Bridge/Beta/HTML/plugin/prismpage.js → wwwroot/js/prismpage.js
```

```html
<!-- master_template.html -->
<script src="js/prismpage.js"></script>
<PrismPage view-page="dashboard.html"></PrismPage>
```

> **Benefit:** Edit subpages in master template context even without running server!

### Template Configuration

```pascal
procedure TFormTemplate.ExportD2Bridge;
begin
  inherited;
  Title := 'D2 Checkin';
  TemplateClassForm := TAdminPageTemplate;
  
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := 'master_template.html';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := 'page_template.html';
end;
```

### Template Class (Prism Form)

```pascal
type
  TAdminPageTemplate = class(TD2BridgePrismForm)
  protected
    procedure Callback(const ACallbackName: string;
      AEventParsing: ID2BridgeEventParsing); override;
    procedure TagHTML(const ATag: string; var ATagValue: string); override;
  end;

procedure TAdminPageTemplate.TagHTML(const ATag: string; var ATagValue: string);
begin
  if SameText(ATag, 'username') then
    ATagValue := D2Checkin.UserName
  else if SameText(ATag, 'user_foto') then
    ATagValue := D2Checkin.UserFoto;  // Base64
end;
```

---

## 🧩 Components

### Component Export

#### Basic Syntax

```pascal
procedure TForm_Checkout.ExportD2Bridge;
begin
  inherited;

  Title := 'My D2Bridge Form';

  //TemplateClassForm := TD2BridgeFormTemplate;
  D2Bridge.FrameworkExportType.TemplateMasterHTMLFile := '';
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := '';

  with D2Bridge.Items.Add do
  begin
    with Row.Items.Add do
      with ColAuto.Items.Add do
      begin
        VCLObj(Label_TagName);
        VCLObj(DBText_TagName);
      end;

    with Row.Items.Add do
      Col8.Add.LCLObj(Image_Product);
  end;
end;
```

#### Layouts (Rows and Columns)

```pascal
// Row with columns
with D2Bridge.Items.Add do
begin
  Row.Items.Add.VCLObj(EditFirstName, CSSClass.Col.ColMD6);
  Row.Items.Add.VCLObj(EditLastName, CSSClass.Col.ColMD6);
end;

// Row with Bootstrap margins
with D2Bridge.Items.Add.Row('mt-2 mb-2') do  // margin-top, margin-bottom
begin
  Row.Items.Add.Image(ImageFoto, CSSClass.Col.ColMD4);
  Row.Items.Add.FileUpload(UploadFoto, CSSClass.Col.ColMD8);
end;
```

### Bootstrap CSS Classes (Columns)

| Class | Description |
|-------|-------------|
| `ColMD1` - `ColMD12` | 1-12 units on medium+ |
| `ColLG1` - `ColLG12` | 1-12 units on large+ |
| `ColSize1` - `ColSize12` | Fixed width |

### Panel Group

```pascal
with D2Bridge.Items.Add.PanelGroup('{{_personal_data_}}', 'PanelUserData', 
                                    CSSClass.Col.ColLG12) do
begin
  AddFormGroup('{{_name_}}', CSSClass.Col.ColLG4).AddVCLObj(DBEditName);
  AddFormGroup('{{_email_}}', CSSClass.Col.ColLG4).AddVCLObj(DBEditEmail);
  AddFormGroup('{{_phone_}}', CSSClass.Col.ColLG4).AddVCLObj(DBEditPhone);
end;
```

### Card

```pascal
with Card do
begin
  CSSClasses := CSSClass.Card.Card_Center_ExtraLarge;

  Header('My Text');

  with BodyItems.Add do
  begin
    with Row.Items.Add do
    begin
      Col6.Add.FormGroup(Label_FullName).AddLCLObj(DBEdit_FullName, 'ValidationAccount', true);
      Col6.Add.FormGroup(Label_Doc).AddLCLObj(DBEdit_Doc);
    end;
    // ...
  end;

  with Footer.Items.Add do
    with Row.Items.Add do
    begin
      ColAuto.Add.LCLObj(Button_Save, 'ValidationAccount', false, CSSClass.Button.save);
    end;
end;
```

### Div

```pascal
with D2Bridge.Items.Add do
begin
  with HTMLDIV('myClass').Items.Add do
  begin
    // Div content
    VCLObj(Component);
  end;
end;
```

### Accordion

```pascal
with D2Bridge.Items.Add.Accordion do
begin
  // Section expanded by default
  with Items.Add('Photos', True) do
    Items.Add.Image(Image1);
  
  // Section collapsed by default
  with Items.Add('Advanced Options', False) do
    Items.Add.VCLObj(EditAdvanced);
end;
```

### Carousel

```pascal
// Carousel with DataSource - ONE LINE!
with Row.Items.Add.Div(CSSClass.Col.ColMD4) do
begin
  Carousel
    .DataSource(DSEventPhoto)
    .FieldName('foto_path');
end;

// Configuration
with Carousel do
begin
  DataSource(DSEventPhoto);
  FieldName('foto_path');
  Interval := 3000;        // ms between slides
  MaxRecords := 25;        // max images
  ShowIndicators := True;  // indicator dots
  ShowControls := True;    // prev/next buttons
  AutoPlay := True;        // auto-scroll
end;
```

### Link Component

Turns any VCL component into a clickable link:

```pascal
// Regular component
.AddVCLObj(DBTextAdminName);

// Component as LINK
.AddVCLObjLink(DBTextAdminName);
```

```pascal
// Click handling
procedure TFormAccount.DBTextAdminNameClick(Sender: TObject);
begin
  // Open popup
  D2D.ShowPopup('PopupUserAdmin');
end;
```

---

## 📝 Forms

### Creating a Form

```
File → New → Other → D2Bridge Framework → D2Bridge Form
```

### Form Inheritance

```
File → New → Other → D2Bridge Framework → D2Bridge Inherited Form
→ Select Ancestor: TFormTemplate
```

### Form Hierarchy

```
TFormTemplate (Master/Page)
       │
       ├── TFormDashboard
       │
       └── TFormCrudTemplate
                │
                ├── TFormUsers
                ├── TFormEvents
                └── TFormAccount
```

### Export in Inherited Forms

```pascal
procedure TFormDashboard.ExportD2Bridge;
begin
  // ⚠️ In inherited forms COMMENT OUT inherited!
  // inherited;  // Master and Page already in parent!
  
  D2Bridge.FrameworkExportType.TemplatePageHTMLFile := 'dashboard.html';
end;
```

### Form Lifecycle

```
1. Create         → Instance creation
2. ExportD2Bridge → Component export
3. OnShow         → Before rendering (open Query)
4. BeginRender    → Start rendering
5. [Component rendering]
6. EndRender      → End rendering (PrismControlFromID!)
7. [Send HTML to browser]
8. OnActivate     → After loading in browser
```

| Event | When to use | PrismControl available? |
|-------|-------------|------------------------|
| `OnShow` | Opening Query, data | ❌ NO |
| `BeginRender` | Before render | ❌ NO |
| `EndRender` | **PrismControl manipulation** | ✅ YES |
| `OnActivate` | After page load | ✅ YES |

---

## 📊 CRUD System

### CRUD Structure

```
┌─────────────────────────────────────────┐
│ CARD SEARCH (Search)                    │
│  ├── Crud_RowSearch (buttons)           │
│  └── Crud_RowGrid (table)               │
├─────────────────────────────────────────┤
│ CARD DATA (Edit)                        │
│  └── Form fields                        │
└─────────────────────────────────────────┘
```

### CRUD Events

```pascal
type
  TFormUsers = class(TFormCrudTemplate)
  protected
    procedure OnCrudOpen(Sender: TObject); override;
    procedure OnCrudSearch(Sender: TObject); override;
    procedure OnCrudEdit(Sender: TObject; var AAllow: Boolean); override;
    procedure OnCrudInsert(Sender: TObject; var AAllow: Boolean); override;
    procedure OnCrudSave(Sender: TObject; var AAllow: Boolean); override;
    procedure OnCrudDelete(Sender: TObject; var AAllow: Boolean); override;
    procedure OnCrudBack(Sender: TObject); override;
    procedure OnCrudClose(Sender: TObject); override;
  end;
```

### Implementation Example

```pascal
procedure TFormUsers.OnCrudOpen(Sender: TObject);
begin
  CrudOperation(coSearch);
end;

procedure TFormUsers.OnCrudSearch(Sender: TObject);
begin
  DM.QueryUsers.Close;
  DM.QueryUsers.SQL.Text := 
    'SELECT * FROM users WHERE id_account = ' + D2Checkin.AccountID.ToString;
  DM.QueryUsers.Open;
end;

procedure TFormUsers.OnCrudSave(Sender: TObject; var AAllow: Boolean);
begin
  AAllow := True;
  
  // Validation
  if not TD2BridgePoni.IsValidAddress(DBEditEmail.Text) then
  begin
    D2D.Validation(DBEditEmail, False, D2Bridge.LApp.MSG_InvalidEmail);
    AAllow := False;
    Exit;
  end;
end;
```

### CRUD Operations

```pascal
CrudOperation(coSearch);  // Search
CrudOperation(coEdit);    // Edit
CrudOperation(coInsert);  // Insert
CrudOperation(coSave);    // Save
CrudOperation(coDelete);  // Delete
CrudOperation(coBack);    // Back
```

### Start Card

```pascal
CrudStartCard := csSearch;  // Start from list
CrudStartCard := csData;    // Start from edit (e.g., Profile)
```

### Permissions

```pascal
type
  TCrudPermission = set of (cpSearch, cpEdit, cpInsert, cpSave, cpDelete);

CrudPermissions := [cpSearch, cpEdit, cpInsert, cpSave, cpDelete];  // Full
CrudPermissions := [cpSearch];  // View only
```

### Adding Buttons to GRID

```pascal
procedure TFormEvents.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;
  
  if PrismControl.VCLComponent = DBGridSearch then
  begin
    // Add column with buttons
    with PrismControl.Columns.Add do
    begin
      Index := 0;
      Width := 78;  // ~26px per button
      
      // View button
      with Buttons.Add do
      begin
        ButtonModel := bmView;
        Caption := '';
        OnClick := ShowEvent;
      end;
      
      // Edit button
      with Buttons.Add do
      begin
        ButtonModel := bmEdit;
        Caption := '';
        OnClick := procedure begin CrudOperation(coEdit); end;
      end;
      
      // Delete button
      with Buttons.Add do
      begin
        ButtonModel := bmDelete;
        Caption := '';
        OnClick := procedure begin CrudOperation(coDelete); end;
      end;
    end;
  end;
end;
```

---

## 🗄 Database

### DataModule for Session

```pascal
unit Unit_DM;

uses D2Bridge.Instance;

type
  TDM = class(TDataModule)
    ADOConnection: TADOConnection;
    QueryUser: TADOQuery;
    QueryAccount: TADOQuery;
    DSUser: TDataSource;
  end;

function DM: TDM;

implementation

function DM: TDM;
begin
  Result := TDM(D2BridgeInstance.GetInstance(TDM));
end;
```

### Creating DM Instance

```pascal
uses
  Unit_DM,
  Winapi.ActiveX;  // Required for SQL Server!

procedure TD2Checkin.OpenDM;
begin
  CoInitializeEx(0, COINIT_MULTITHREADED);  // SQL Server only!

  if DM = nil then
    D2BridgeInstance.CreateInstance(TDM);
end;
```

### Dynamic Connection Configuration

```pascal
procedure TDM.ADOConnectionWillConnect(Connection: TADOConnection;
  var ConnectionString, UserID, Password: WideString;
  var ConnectOptions: TConnectOption; var EventStatus: TEventStatus);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Config.ini');
  try
    ConnectionString := 
      'Provider=SQLOLEDB.1;' +
      'Password=' + IniFile.ReadString('Database', 'Password', '') + ';' +
      'User ID=' + IniFile.ReadString('Database', 'User', 'SA') + ';' +
      'Initial Catalog=' + IniFile.ReadString('Database', 'Database', 'D2Checkin') + ';' +
      'Data Source=' + IniFile.ReadString('Database', 'Host', 'localhost');
  finally
    IniFile.Free;
  end;
end;
```

### ADO Settings for SQL Server

```
Location: clUseServer     (Cursor Location = Server)
CursorType: ctKeySet      (Cursor Type = KeySet)
```

---

## 📧 Email API

### Configuration

```pascal
procedure TServerController.ConfigEmail;
begin
  if D2BridgeManager.API.Mail.Config then
  begin
    Config.Host := 'smtp.gmail.com';
    Config.Port := 587;
    Config.Username := 'email@gmail.com';
    Config.Password := 'xxxx xxxx xxxx xxxx';  // App Password!
    Config.UseSSL := True;
    Config.UseTLS := True;
    Config.UseThread := True;
    Config.FromName := 'D2 Checkin';
    Config.FromMailAddress := 'noreply@d2checkin.com';
  end;
end;
```

### Required DLLs

```
libeay32.dll
ssleay32.dll
```

### Sending Email

```pascal
procedure TD2Checkin.SendEmail(const ATo, ASubject, ABody: string);
begin
  with D2BridgeServerController.API.Mail do
  begin
    Addresses.Clear;
    Addresses.Add(ATo);
    Subject := ASubject;
    Body.Text := ABody;
    
    if not Send then
      raise Exception.Create('Email sending error');
  end;
end;
```

---

## 🔑 Token System

### Token Types

```pascal
type
  TTokenType = (
    ttNewUser,      // New user (email verification)
    ttRecoverPass,  // Password recovery
    ttNewEvent,     // New event
    ttCheckin       // Check-in
  );
  
  TTokenMode = (
    tmLink,    // Validation by link (64 characters)
    tmCode     // Validation by code (6 digits)
  );
```

### Constants

```pascal
const
  TOKEN_LINK_SIZE = 64;
  TOKEN_CODE_SIZE = 6;
  TOKEN_LINK_EXPIRY_HOURS = 24;
  TOKEN_CODE_EXPIRY_SECONDS = 300;
```

### Creating Token

```pascal
function TD2Checkin.CreateToken(AType: TTokenType; AMode: TTokenMode; 
  AUserID: Integer; AEventID: Integer = 0): Integer;
var
  TokenValue: string;
begin
  repeat
    if AMode = tmLink then
      TokenValue := GenerateRandomString(TOKEN_LINK_SIZE)
    else
      TokenValue := GenerateRandomNumber(TOKEN_CODE_SIZE);
  until not TokenExists(TokenValue);
  
  with DM.QueryToken do
  begin
    Append;
    FieldByName('user_id').AsInteger := AUserID;
    FieldByName('id_event').AsInteger := AEventID;
    FieldByName('type').AsString := TokenTypeToStr(AType);
    FieldByName('token').AsString := TokenValue;
    FieldByName('created_at').AsDateTime := Now;
    FieldByName('expires_at').AsDateTime := Now + (TOKEN_LINK_EXPIRY_HOURS / 24);
    Post;
    Result := FieldByName('id').AsInteger;
  end;
end;
```

---

## 🪟 Popup and Nested Forms

### Creating Popup

```pascal
// In export
with D2Bridge.Items.Add.Popup('PopupName', 'Title', False) do
begin
  // False = show X button
  // True = hide X button
  
  Items.Add.VCLObj(Component1);
  Items.Add.VCLObj(Component2);
end;
```

### Showing/Hiding Popup

```pascal
// Show
D2D.ShowPopup('PopupName');

// Hide
D2D.ClosePopup('PopupName');
```

### Nested Forms

A Nested Form is a form embedded in another form or popup.

```pascal
type
  TFormAccount = class(TFormTemplate)
  private
    FormUsers: TD2BridgeForm;  // ⚠️ Base type (avoid circular reference!)
  end;

procedure TFormAccount.ExportD2Bridge;
begin
  inherited;
  
  // Creating Nested
  if FormUsers = nil then
  begin
    FormUsers := TFormUsers.Create(Self);
    TFormUsers(FormUsers).CrudStartCard := csData;
    D2D.AddNested(FormUsers);
  end;
  
  // Popup with Nested
  with D2Bridge.Items.Add.Popup('PopupUserAdmin', D2Bridge.LApp.edit_user, False) do
  begin
    Items.Add.AddNested(FormUsers);
  end;
end;
```

### Popup Tag in HTML (Landing Page)

```html
<!-- Before </body> -->
<PrismPopup name="PopupShowEvent"></PrismPopup>
```

---

## 📤 File Upload

### Single Upload

```pascal
// Export
Row.Items.Add.FileUpload(UploadFoto, CSSClass.Col.ColMD8);

// Event
procedure TFormUsers.UploadFotoUpload(Sender: TObject);
var
  UploadedFile, NewFileName: string;
begin
  UploadedFile := UploadFoto.UploadFiles[0];
  
  // Generate unique name (cache busting!)
  NewFileName := D2Checkin.FILE_PATH_IMAGES + 
                 'user_' + UserID.ToString + '_' +
                 GenerateRandomString(8) + 
                 ExtractFileExt(UploadedFile);
  
  // Copy new one
  CopyFile(PChar(UploadedFile), PChar(NewFileName), False);
  
  // Load preview
  ImageFoto.Picture.LoadFromFile(NewFileName);
end;
```

### Multiple File Upload

```pascal
// Export with limit
with Row.Items.Add.FileUpload(UploadImages, CSSClass.Col.ColMD12) do
begin
  MaxFiles := 5;         // Max 5 files
  MaxFileSize := 20;     // Max 20 MB per file
  Accept := 'image/*';   // Images only
end;
```

---

## ✅ Validation

### Email Validation

```pascal
uses D2Bridge.Poni;

if not TD2BridgePoni.IsValidAddress(EditEmail.Text) then
begin
  D2D.Validation(EditEmail, False, D2Bridge.LApp.MSG_InvalidEmail);
  Abort;
end;
```

### Field Masks

```pascal
procedure TFormAccount.InitControlsD2Bridge(const PrismControl: TPrismControl);
begin
  inherited;
  
  // CNPJ mask (Brazil)
  if PrismControl.VCLComponent = DBEditDocument then
    TDBEdit(PrismControl.VCLComponent).EditMask := TD2BridgePrismTMask.BrasilCNPJ;
  
  // Custom mask
  if PrismControl.VCLComponent = DBEditPhone then
    TDBEdit(PrismControl.VCLComponent).EditMask := 
      '{"mask": "(99) 99999-9999", "autoUnmask": true}';
end;
```

---

## 🔒 Sessions and Security

### Session Initialization

```pascal
// ServerController.pas
procedure TServerController.NewSession(Sender: TObject);
begin
  D2Checkin.IsLogged := False;
  D2Checkin.AutoJoinEventID := 0;
end;
```

### Login

```pascal
procedure TFormLogin.ButtonLoginClick(Sender: TObject);
begin
  // Validation...
  
  // On success:
  D2Checkin.IsLogged := True;
  D2Checkin.UserID := DM.QueryUser.FieldByName('id').AsInteger;
  
  FormDashboard.Show;
end;
```

### Closing Session

```pascal
// Ends EVERYTHING: threads, forms, DM, variables
Session.Close;
// or Session.Close(false/true);
```

### File Security

```
wwwroot/        ← Accessible from browser
files/          ← NOT accessible from browser!
```

---

## 🔄 Application Lifecycle

### Form Events

```
Create → ExportD2Bridge → OnShow → BeginRender → 
[Rendering] → EndRender → InitControlsD2Bridge → 
[Send to browser] → OnActivate
```

### When to Use Which Event?

| Event | Usage |
|-------|-------|
| `OnShow` | Opening Query, setting data |
| `BeginRender` | Preparation before render |
| `EndRender` | `D2D.PrismControlFromID()` |
| `InitControlsD2Bridge` | Masks, GRID formatting |
| `OnActivate` | User questions, auto-actions |

### ⚠️ Important Restrictions

```pascal
// ❌ NEVER in OnShow/BeginRender/EndRender:
ShowMessage('Question?', mtConfirmation, [mbYes, mbNo]);
// Will block session!

// ✅ Only in OnActivate:
procedure TForm.FormActivate(Sender: TObject);
begin
  if ShowMessage('Continue?', mtConfirmation, [mbYes, mbNo]) = mrYes then
    // ...
end;
```

---

## 💡 Best Practices

### 1. Compilation

```
✅ DEBUG      → Generates JSON translations
✅ RELEASE    → Production
```

### 2. Units Order

```pascal
uses
  // ...other units...
  D2Bridge.Forms;  // ← ALWAYS LAST!
```

### 3. UTF-8 for Emails

```pascal
EmailBody.LoadFromFile(Path, TEncoding.UTF8);
```

### 4. OnActivate for Query with DBLookupComboBox

```pascal
// WRONG - components not rendered yet
procedure TForm.FormShow(Sender: TObject);
begin
  DM.Query.Open;  // DBLookup won't work!
end;

// CORRECT
procedure TForm.FormActivate(Sender: TObject);
begin
  DM.Query.Open;  // Now it works!
end;
```

### 5. Avoiding Circular Reference

```pascal
// WRONG
FormUsers: TFormUsers;  // Concrete type

// CORRECT
FormUsers: TD2BridgeForm;  // Base type
TFormUsers(FormUsers).Property := Value;  // Cast
```

### 6. Auto-Fill in Debug

```pascal
procedure TFormLogin.FormActivate(Sender: TObject);
begin
  if DebugHook <> 0 then
  begin
    EditEmail.Text := 'test@test.com';
    EditPassword.Text := 'password';
  end;
end;
```

### 7. TLS/SSL Encrypted Connections

D2Bridge supports encrypted TLS connections. To enable TLS, you must:

1. Add `IdSSLOpenSSLHeaders` to your `uses` clause.
2. Point OpenSSL to the directory containing the required DLLs (`ssleay32.dll` and `libeay32.dll`):

```pascal
uses
  IdSSLOpenSSLHeaders;

// Set the path to SSL libraries (must match your target architecture)
IdOpenSSLSetLibPath('C:\Path\To\SSL\DLLs');
```

> **Note:** TLS will not work if `IdSSLOpenSSLHeaders` is missing from the `uses` clause or if the SSL library path is not set correctly.

### 8. Auto-Translation of GUI Navigation Elements

To enable automatic translation of built-in GUI navigation elements (e.g. button captions), use context-aware translation tags in your captions:

```
{{_Button,CaptionOpen_}}
{{_Button,CaptionRefresh_}}
```

Then add `D2Bridge.Lang.Core` to your `uses` clause and override `TagTranslate`:

```pascal
uses
  D2Bridge.Lang.Core;

procedure TForm1.TagTranslate(const Language: TD2BridgeLang;
  const AContext: string; const ATerm: string; var ATranslated: string);
begin
  inherited;
  ATranslated := D2BridgeLangCore.LangByTD2BridgeLang(Language)
    .Language.Translate(AContext, ATerm);
end;
```

---

## 🔧 Troubleshooting

### "Pointer is not valid" when closing session

**Cause:** Normal in debug mode when closing threads.  
**Solution:** Ignore in debug, doesn't appear in release.

### DBLookupComboBox doesn't show data

**Cause:** Query opened in `OnShow` instead of `OnActivate`.  
**Solution:** Move `Query.Open` to `OnActivate`.

### Images don't refresh after upload

**Cause:** Browser cache.  
**Solution:** Add random string to filename.

### PrismControlFromID generates exception

**Cause:** Called in `OnShow` instead of `EndRender`.  
**Solution:** Move to `EndRender` or `OnActivate`.

### Popup doesn't appear in Landing Page

**Cause:** Missing `<PrismPopup>` tag in HTML.
**Solution:** Add before `</body>`:
```html
<PrismPopup name="PopupName"></PrismPopup>
```

### Browser requests redirect to d2bridge.com.br

**Cause:** No `favicon.ico` file in the `wwwroot/` directory. The framework issues a 301 redirect to `https://d2bridge.com.br/favicon.ico`.
**Solution:** Place your own `favicon.ico` file in the `wwwroot/` directory.

---

## 📚 Sample Project: D2Checkin

### Description

Event management system with participant check-in.

### D2Bridge Mechanics Used

- ✅ Translation system (18 languages)
- ✅ Callbacks with parameters
- ✅ Master/Page Template
- ✅ CRUD with inheritance
- ✅ Single and multiple upload
- ✅ Field masks
- ✅ Link component
- ✅ Nested Forms
- ✅ Popup
- ✅ Carousel
- ✅ Tokens
- ✅ Toast
- ✅ Clipboard
- ✅ GRID formatting
- ✅ Validation
- ✅ Email API

---

## 📖 Additional Resources

### Official

- [GitHub Repository](https://github.com/d2bridge/d2bridgeframework)
- [Discord Server](https://discord.gg/WvHaWP6h9t)
- [Official Website](https://d2bridge.com.br)


---

<div align="center">

**"Develop web the Delphi way"**

*D2Bridge Framework - LGPL 2.1*

</div>
