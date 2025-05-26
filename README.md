<p align="center">
  <img src="https://github.com/user-attachments/assets/ec398da9-c462-40e8-b578-c3c264965042" alt="BuilderUI">
</p>

# BuilderUI - Forms Builder UI for Windows 

## Overview
BuilderUI is a powerful tool for creating JSON-based graphical user interfaces for Windows. The project allows dynamic creation of forms through JSON definitions, with support for export to multiple platforms.

## Features

- **JSON-Based Form Creation**
  - Create forms dynamically using JSON definitions
  - Real-time form preview
  - Component property customization
  - Nested control support

- **Multi-Platform Export**
  - Delphi Forms
  - C# Windows Forms
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

## Architecture

The project follows a clean architecture pattern with the following main components:

- **Core Layer**
  - `Core.IUIBuilder`: Core interface for form building operations
  - `Builder.UIBuilderEngine`: Main engine for form creation and manipulation

- **Adapters**
  - `Adapter.TreeViewAdapter`: Handles component tree visualization

- **Services**
  - `Services.AIService`: AI integration for form generation

- **Utils**
  - `Util.JSON`: JSON handling and validation utilities

## JSON Structure Example

```json
{
  "Name": "MainForm",
  "Caption": "Sample Form",
  "Width": 800,
  "Height": 600,
  "Controls": [
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

## Contributing

Contributions are welcome! Please feel free to submit pull requests, create issues or suggest improvements.

## License

This project is open source and available under the MIT License.

---

*Note: This README provides an overview of the BuilderUI project. For detailed documentation and examples, please refer to the project documentation.*

        
