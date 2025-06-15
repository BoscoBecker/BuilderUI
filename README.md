> ⚠️ **BuilderUI is under active development.** Features, structure, and APIs may change frequently.

<p align="center">
  <img src="https://github.com/user-attachments/assets/5589f7b6-1141-4839-b95e-1a98ff2206dd" alt="BuilderUI">
</p>

# 🧩 BuilderUI - (Design Patterns Applied)
### A JSON-based Form Designer for Windows with Modern Design Patterns

## 📌 Overview

**BuilderUI** is a dynamic UI builder that empowers developers to design, preview, and manage Windows Forms entirely through structured JSON definitions. With an intuitive visual editor and real-time rendering, it simplifies the creation of complex forms without manual coding.

Key capabilities include:

- 🔧 **Visual Design Tools**: inspection and live form preview  
- 📄 **JSON-Driven UI**: Define entire form structures and components declaratively via JSON  
- 🚀 **Multi-Platform Export**: Export your forms to Delphi, Lazarus, C# (.NET 9), and Java (Swing/AWT)  
- 🧩 **Component Customization**: Full control over properties, styles, and layout  
- 📐 **Modern Architecture**: Clean structure using patterns like Factory, Strategy, SRP, and Adapter  

> Whether you're prototyping UIs or building full applications, BuilderUI offers flexibility, portability, and extensibility — all powered by JSON.


![image](https://github.com/user-attachments/assets/ec46c849-19e0-4781-bbc8-21b6d1f65e9f)



## Features

- **JSON-Based Form Creation**
  - Create forms dynamically using JSON definitions
  - Real-time form preview
  - Component property customization
  - Nested control support

- **Multi-Platform Export**
  - Delphi Forms 100% 
  - Lazarus Forms 100%
  - C# Windows Forms - dotnet 9 : 100%
  - Java Swing/AWT

- **Visual Design Tools**
  - Component palette
  - Property inspector (TreeView)
  - Visual form designer
  - Multi-select support X not
  - Drag and drop functionality X not 

- **Advanced Features**
  - Component tree view
  - JSON validation
  - Real-time preview
  - Grid alignment system
  - Component search functionality
  - Syntax Highlighting with SynEdit

## 📐 Design Patterns & Project Architecture
BuilderUI applies several classic and modern design patterns to maintain a clean, scalable, and maintainable architecture. The system is modular and organized into well-defined layers, each with a specific responsibility, embracing SOLID principles.

🔧 Applied Patterns

| Pattern                  | Description                                                                            | Examples / Files                                                                                                     |
| ------------------------ | -------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------- |
| **Factory Pattern**      | Encapsulates the instantiation of code generators per platform.                        | `Factory.ICodeGenerator`, `Factory.CodeGeneratorFactory`, `Factory.CodeGenerator.Delphi/CSharp/Lazarus`              |
| **Strategy Pattern**     | Decouples export logic into swappable algorithms per target platform.                  | `Strategy.IExport`, `Strategy.Export.Delphi`, `Strategy.Export.CSharp`, `Strategy.Export.Lazarus`                    |
| **Builder Pattern**      | Builds complex form structures from JSON definitions step by step.                     | `Builder.UIBuilderEngine`, `Core.IUIBuilder`                                                                         |
| **Adapter Pattern**      | Adapts third-party or incompatible APIs like TreeView and SynEdit into the system.     | `Adapter.TreeViewAdapter`, `Adapter.SynEditAdapter`                                                                  |
| **Service Layer**        | Isolates business logic into independent services.                                     | `Service.Zoom`, `Service.Export`, `Service.Json.Validation`, `Service.Component.Search`, `Service.StatusBar.Manager` |
| **Singleton (manual)**   | Provides a single shared instance for user configuration or global settings.           | `Builder.UI.UserPreferences`                                                                                         |
| **Decorator (implicit)** | Enhances or extends component behavior through wrapping or helpers.                    | Applied via class helpers and component managers                                                                     |
| **Observer (partial)**   | Used in form lifecycle handling (e.g., `OnClose`) and UI updates (e.g., zoom display). | `FormBuilderMain`, `Service.Forms.Manager`, `Service.StatusBar.Manager`                                              |
| **Facade (technical)**   | Central point of coordination between services, forms, adapters, and UI.               | `FormBuilderMain` acts as the entry point and orchestrator                                                           |

## 🗂️ Layered Architecture Overview

### 🔹 Core Layer
Defines interfaces and base contracts  
- `Core.IUIBuilder`

### 🔹 Builder Layer
Main engine to create forms  
- `Builder.UIBuilderEngine`  
- `Builder.UI.UserPreferences`

### 🔹 Adapter Layer
Integrates third-party or GUI systems  
- `Adapter.TreeViewAdapter`  
- `Adapter.SynEditAdapter`

### 🔹 Service Layer
Independent business logic and tools  
- `Service.Zoom`  
- `Service.Export`  
- `Service.Forms.Manager`  
- `Service.Json.Validation`  
- `Service.StatusBar.Manager`  
- *(and others)*

### 🔹 Utility Layer
Shared helpers and tools  
- `Util.JSON`  
- `Util.Form.Arranger`  
- `Util.JSONValidator`  
- `Enum.Utils`

### 🔹 Factory Layer
Platform-specific form generators  
- `Factory.CodeGenerator.Delphi`  
- `Factory.CodeGenerator.CSharp`  
- `Factory.CodeGenerator.Lazarus`

### 🔹 Strategy Layer
Export logic for each platform  
- `Strategy.Export.Delphi`  
- `Strategy.Export.CSharp`  
- `Strategy.Export.Lazarus`

### 🔹 View Layer
Application interface and screens  
- `View.Builder.Main`  
- `View.Export.Forms`  
- `View.Menu.Context.Windows`  
- `View.Window.Json`


## JSON Structure Example

```json
{
  "Type": "TForm",
  "Name": "FrmMainForm",
  "Caption": "Sample Form",
  "Width": 800,
  "Height": 600,
  "Children": [
    {
      "Type": "TPanel",
      "Name": "Panel1",
      "Left": 10,
      "Top": 10,
      "Width": 200,
      "Height": 100,
      "Color": "#FFFFFF"
    }
  ]
}
```

# ✨ AI Integration & Copilot Usage to create rich UI ✨

## Using GitHub Copilot in VS Code for JSON Editing

You can leverage GitHub Copilot in Visual Studio Code to quickly generate and edit JSON files for BuilderUI:

1. **Open your project folder in VS Code.**
2. **Open or create a `.json` file** inside the `/src/Json` directory (e.g., `MyForm.json`).
3. **Use Copilot Chat or inline suggestions** to request or modify JSON structures.
   - For example, type a comment like:
     ```json
     // Generate a JSON for a login form with user, password fields and a login button based in BuilderUI pattern.
     ```
   - Or ask in Copilot Chat:
     ```
     Generate a product registration form with name, price, category (combobox), and save/cancel buttons based in BuilderUI pattern.
     ```
4. **Accept Copilot's suggestion** or edit as needed.
5. **Save the file.** Having your forms as `.json` files makes it much easier to edit, refactor, and reuse them with Copilot's help.

### Tips

- Saving your JSON files allows Copilot to provide better context and more accurate suggestions.
- You can easily modify, extend, or refactor your forms by editing the JSON and letting Copilot assist with repetitive or boilerplate structures.
- Use the provided JSON examples as templates for your own screens.

# ✨ <img src="https://github.com/user-attachments/assets/41151db6-1fd1-42f1-b38b-a96943388fa5" width="30" />  JSON Usage to create rich UI ✨

- Advanced JSON converter and formatter. Transforms data into a structured, precise JSON format. Also exports JSON in a CSV file.

You can Using https://chatgpt.com/g/g-bIMOi37Fy-json

**Open or create a `.json` file** inside the `/src/Json` directory (e.g., `MyForm.json`), send a context and later  the question


### Future - Integrated AI Chat

> The idea is to use an integrated chat to assist users in generating, editing, and refactoring JSON UI definitions. This chat-based AI can help automate repetitive tasks, provide instant suggestions, and accelerate the UI design process directly within your development environment. Coming soon, need help.

## Getting Started

1. Clone the repository
2. Open the project in Delphi
3. Build and run the application
4. Use the JSON editor to create your form
5. Preview the results in real-time
6. Export to your desired platform

## Requirements

- Delphi IDE (Recent versions)
- Windows Operating System
- Skia4Delphi components
- Synedit

## Contributing

Contributions are welcome! Please feel free to submit pull requests, create issues or suggest improvements.

## License

This project is open source and available under the GNU License.

---

*Note: This README provides an overview of the BuilderUI project. For detailed documentation and examples, please refer to the project documentation.*


