# Extractor

Open Source: https://github.com/BazzaCuda/Extractor

Reads through a list of passwords, determines which password (if any) applies to each archive, and extracts to a subfolder in the same folder as the archive.

In the .ini file, DLPath=\<some folder\> supplies a base folder for the "Find Files" function, typically your downloads folder.
You can also drag and drop files from any folder (or multiple folders) from your File Explorer onto the Extractor window to add files to the list. Holding down the SHIFT key when you drop the files will replace the current list.

If you click a row in the grid, pressing DELETE will delete that row. It doesn't delete the archive file; Extractor contains no file deletion code whatsoever.

Clicking a cell in the grid will copy the contents to the clipboard.

Passwords are contained in the passwords.txt file which is UTF-8 file to allow for special characters. If you edit passwords.txt, make sure your editor keeps the file UTF-8 encoded. You can edit this file and check the Reload PWs checkbox. The file will be reloaded on the next "Find PWs" or "Extract" operation. This saves you having to restart the app between edits. 

If you use the add password box, Extractor will maintain the UTF-8 encoding of the passwords.txt file automatically.


 
![extractor 2025-02-26_115104](https://github.com/user-attachments/assets/3612f2a1-4421-442e-8c39-aaec0960d4f0)

# Build Dependencies
Mormot2 - OpenSource RESTful ORM/SOA/MVC ToolBox for Delphi and FreePascal: https://github.com/synopse/mORMot2, which contains an excellent Delphi wrapper for 7z.dll

Baz's Debug Window - OpenSource: https://github.com/BazzaCuda/BazDebugWindow, an enhanced reworking of the original DebugWindow by GExperts
