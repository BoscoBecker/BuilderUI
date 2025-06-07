# BuilderUI Delphi

This project is a BuilderUI built in Delphi, which renders graphical interfaces from JSON files. The goal is to make it easier to create screens and forms dynamically, without the need to manually edit Delphi source code.

## How it works

- BuilderUI reads `.json` files that describe forms, components, and their properties.
- Each JSON file represents one or more screens (Forms) and their components (Buttons, Edits, Labels, Panels, etc).
- Delphi renders the interface according to the structure defined in the JSON.

## Why use VS Code + Copilot

Since Delphi does not have an efficient integrated chat to generate these JSON files, you can use VS Code with GitHub Copilot to quickly generate interface JSON files, just by describing the screen you want.

## How to ask Copilot to generate a JSON

1. Open VS Code in the project folder.
2. Open the Copilot chat.
3. Send the context below (copy the "Context for Copilot" block).
4. Then, ask questions or requests such as:  
   - "Generate a JSON for a product registration form with fields: name, price, category, save and cancel buttons."
   - "Add a phone field to the customer form."
5. Copilot will generate the JSON in the correct pattern for you to paste into the project.

---

## Context for Copilot

Copy and send this block to Copilot before requesting file generation:

This is a BuilderUI software in Delphi that renders screens from JSON files.
The pattern for the JSON files is similar to this example:

```json
///This is a BuilderUI software in Delphi that renders screens from JSON files. The JSON file pattern is similar to this example, BuilderUI standard:
{
  "Forms": [
    {
      "Name": "FrmCustomerRegistration",
      "Caption": "Customer Registration",
      "Type": "TForm",
      "Align": "Client",
      "Children": [
        {
          "Name": "LblTituloCliente",
          "Type": "TLabel",
          "Text": "Customer Registration",
          "Position": { "X": 5, "Y": 10 },
          "Width": 300,
          "Height": 30,
          "FontSize": 16,
          "FontStyle": ["Bold"]
        },
        {
          "Name": "LblNomeCompleto",
          "Type": "TLabel",
          "Text": "Full Name:",
          "Position": { "X": 5, "Y": 50 },
          "Width": 300,
          "Height": 20
        },
        {
          "Name": "EdtNomeCompleto",
          "Type": "TEdit",
          "Text": "",
          "Position": { "X": 5, "Y": 70 },
          "Width": 300,
          "Height": 30
        },
        {
          "Name": "LblTelefone",
          "Type": "TLabel",
          "Text": "Phone:",
          "Position": { "X": 5, "Y": 110 },
          "Width": 300,
          "Height": 20
        },
        {
          "Name": "EdtTelefone",
          "Type": "TEdit",
          "Text": "",
          "Position": { "X": 5, "Y": 130 },
          "Width": 300,
          "Height": 30
        },
        {
          "Name": "BtnSalvarCliente",
          "Type": "TButton",
          "Caption": "Save",
          "Position": { "X": 5, "Y": 180 },
          "Width": 140,
          "Height": 40
        },
        {
          "Name": "BtnCancelarCliente",
          "Type": "TButton",
          "Caption": "Cancel",
          "Position": { "X": 165, "Y": 180 },
          "Width": 140,
          "Height": 40
        }
      ]
    },
    {
      "Name": "FrmAddressCustomer",
      "Caption": "Customer Address",
      "Type": "TForm",
      "Align": "Client",
      "Children": [
        {
          "Name": "LblTituloEndereco",
          "Type": "TLabel",
          "Text": "Customer Address",
          "Position": { "X": 5, "Y": 10 },
          "Width": 300,
          "Height": 30,
          "FontSize": 16,
          "FontStyle": ["Bold"]
        },
        {
          "Name": "LblLogradouro",
          "Type": "TLabel",
          "Text": "Street:",
          "Position": { "X": 5, "Y": 50 },
          "Width": 300,
          "Height": 20
        },
        {
          "Name": "EdtLogradouro",
          "Type": "TEdit",
          "Text": "",
          "Position": { "X": 5, "Y": 70 },
          "Width": 300,
          "Height": 30
        },
        {
          "Name": "LblNumero",
          "Type": "TLabel",
          "Text": "Number:",
          "Position": { "X": 5, "Y": 110 },
          "Width": 150,
          "Height": 20
        },
        {
          "Name": "EdtNumero",
          "Type": "TEdit",
          "Text": "",
          "Position": { "X": 5, "Y": 130 },
          "Width": 150,
          "Height": 30
        },
        {
          "Name": "LblCidade",
          "Type": "TLabel",
          "Text": "City:",
          "Position": { "X": 5, "Y": 170 },
          "Width": 150,
          "Height": 20
        },
        {
          "Name": "EdtCidade",
          "Type": "TEdit",
          "Caption": "",
          "Position": { "X": 5, "Y": 190 },
          "Width": 150,
          "Height": 30
        },
        {
          "Name": "BtnSalvarEndereco",
          "Type": "TButton",
          "Caption": "Save Address",
          "Position": { "X": 5, "Y": 240 },
          "Width": 150,
          "Height": 40
        }
      ]
    },
    {
      "Type": "TForm",
      "Name": "FrmButtons",
      "Caption": "Button List",
      "Align": "Client",
      "Children": [
        {
          "Type": "TButton",
          "Name": "BtnLimparCampos",
          "Caption": "Clear Fields",
          "Position": { "X": 5, "Y": 100 },
          "Width": 150,
          "Height": 40
        },
        {
          "Type": "TButton",
          "Name": "BtnGerarPDF",
          "Caption": "Export PDF",
          "Position": { "X": 165, "Y": 150 },
          "Width": 140,
          "Height": 40
        },
        {
          "Type": "TButton",
          "Name": "BtnEnviarEmail",
          "Caption": "Send by Email",
          "Position": { "X": 5, "Y": 200 },
          "Width": 120,
          "Height": 120
        }
      ]
    }
  ]
}
```
Each component has properties such as Type, Name, Position, Width, Height, Caption/Text, Items (for lists), etc.
I want you to generate JSON files following this pattern, for registration, login, search screens, etc.
Always generate the complete JSON, ready to be saved and used in Delphi.

---

## Example questions for Copilot

- Generate a JSON for a login form with user, password fields and a login button.
- Generate a product registration form with name, price, category (combobox), and save/cancel buttons.
- Generate a search form with a grid and filters by name and date.

---

## File structure

- Each JSON file can contain one or more forms (property `"Forms": [...]` or just one object).
- Components are described in `"Children": [...]` inside the form.
- See examples in [AllComponents.json](\src\Json\AllComponents.json).

---

## Notes

- The component pattern follows Delphi naming (TEdit, TButton, TPanel, etc).
- Always use properties like Position, Width, Height, Caption/Text, Items, etc.
- For list components (ComboBox, ListBox), use the `"Items": [...]` property.

---

Ready! Now just ask Copilot to generate the JSON files as needed.