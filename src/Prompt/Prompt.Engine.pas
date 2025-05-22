unit Prompt.Engine;

interface

resourcestring
  SUiBuilderPromptTemplate =
'''
  {
  "instruction": "Generate a new UI layout in the same structure as the example, based on the user request.",
  "format": "json",
  "output_format": "Only return the JSON. Do not include any explanation or description.",
  "types_available": ["TLayout", "TLabel", "TEdit", "TButton"],
  "example":
  {
    "Type": "TLayout",
    "Align": "Client",
    "Children": [
      {
        "Type": "TLabel",
        "Text": "Nome de Cadastral",
        "Position": { "X": 5, "Y": 10 },
        "Width": 300,
        "Height": 40
      },
      {
        "Type": "TEdit",
        "Text": "",
        "Position": { "X": 5, "Y": 50 },
        "Width": 280,
        "Height": 40
      },
      {
        "Type": "TButton",
        "Text": "Salvar",
        "Position": { "X": 5, "Y": 100 },
        "Width": 300,
        "Height": 40
      }
    ]
  },
  "user_request": "Criar uma tela de cadastro de funcionário com nome, cpf, rg, email, cargo e botão de salvar"
  "output_format": "Respond only with JSON. Do not include markdown, code blocks or comments."}
  ''';
























implementation

end.
