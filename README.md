<p align="center">
  <img src="https://github.com/user-attachments/assets/ec398da9-c462-40e8-b578-c3c264965042" alt="BuilderUI">
</p>

# BuilderUI - Forms Builder UI for Windows (Design Patterns Applied)

## Overview
BuilderUI is a powerful tool for creating JSON-based graphical user interfaces for Windows. The project allows dynamic creation of forms through JSON definitions, with support for export to multiple platforms.

![image](https://github.com/user-attachments/assets/4c088d07-b81d-486f-b178-0a91ccc46511)


## Features

- **JSON-Based Form Creation**
  - Create forms dynamically using JSON definitions
  - Real-time form preview
  - Component property customization
  - Nested control support

- **Multi-Platform Export**
  - Delphi Forms 100% need refactor
  - C# Windows Forms 90% 
  - Java Swing/AWT
  - Lazarus Forms

- **Visual Design Tools**
  - Component palette
  - Property inspector
  - Visual form designer
  - Multi-select support
  - Drag and drop functionality

- **Advanced Features**
  - Component tree view
  - JSON validation
  - Real-time preview
  - Grid alignment system
  - Component search functionality
  - **Syntax Highlighting with SynEdit** (new!)

## Architecture

The project follows a clean architecture pattern with the following main components and now adopts additional modern design patterns:

- **Core Layer**
  - `Core.IUIBuilder`: Core interface for form building operations
  - `Builder.UIBuilderEngine`: Main engine for form creation and manipulation

- **Adapters**
  - `Adapter.TreeViewAdapter`: Handles component tree visualization
  - `Adapter.SynEditAdapter`: Integrates SynEdit for advanced code editing and syntax highlighting (new!)

- **Services**
  - `Services.AIService`: AI integration for form generation
  - `Services.ExportService`: Handles export logic, decoupled from UI (new!)

- **Utils**
  - `Util.JSON`: JSON handling and validation utilities
  - `Utils.TreeViewHelper`: Helper for TreeView operations (new!)

- **Patterns**
  - **Factory Pattern**: For code generator instantiation
  - **Strategy Pattern**: For export logic per technology
  - **Class Helpers**: For extending VCL/FMX components
  - **Service Layer**: For business logic separation
  - **SRP (Single Responsibility Principle)**: Enforced in recent refactors

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
      "Color": "#FFFFFF",
      "BevelOuter": "bvNone"
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

### Integrated AI Chat

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

This project is open source and available under the MIT License.

---

*Note: This README provides an overview of the BuilderUI project. For detailed documentation and examples, please refer to the project documentation.*


