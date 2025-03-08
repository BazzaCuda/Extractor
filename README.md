# Extractor

Open Source: https://github.com/BazzaCuda/Extractor

Reads through a list of passwords, determines which password (if any) applies to each archive, and extracts to a subfolder in the same folder as the archive.

Extractor supports all archive formats supported by 7-Zip (7z.dll). 

_Extractor detects the archive format of each file by the file contents, not the file extension._
 
![image](https://github.com/user-attachments/assets/e2063409-4b09-4f4d-b44d-ee6b19a8be98)


In the .ini file, DLPath=\<some folder\> supplies a base folder for the "Find Files" function, typically your downloads folder.
You can also drag and drop files from any folder (or multiple folders) from your File Explorer onto the Extractor window to add files to the list. Holding down the SHIFT key when you drop the files will replace the current list.

You can drag and drop multiple files at the same time and they will all be added to the grid.

If you click a row in the grid, pressing DELETE will delete that row. It doesn't delete the archive file; Extractor contains no file deletion code whatsoever.

Clicking a cell in the grid will copy the contents to the clipboard.

Passwords are contained in the passwords.txt file which is a UTF-8 file to allow for special characters. If you edit passwords.txt, make sure your editor keeps the file UTF-8 encoded. You can edit this file and check the "reload PWs" checkbox. The file will be reloaded on the next "Find PWs" or "Extract" operation. This saves you having to restart the app between edits. 

If you use the add password box, Extractor will maintain the UTF-8 encoding of the passwords.txt file automatically.

In the .ini file, DLExts= provides a comma-separated, or semi-colon-separated list specifying which archive extensions the "Find Files" function should search for in the DLPath folder. e.g. \*.7z,\*.rar,\*.zip

You may drag and drop any archive file format onto the grid - dragged and dropped files are not limited by the extensions you've specified for the "Find Files" function.

The "Extract" function will do a "Find PWs" first. You don't have to do "Find PWs" first and then "Extract".

If a password is found, it will be copied to the top of the internal list so that if subsequent archives use the same password it will be found much quicker the next time. The internal list gets reset each time the app is run.

Extractor outputs a sorted and de-duplicated copy of passwords.txt called "passwords_deduped.txt".

# Known Issues
The 7z.dll API doesn't seem to handle split archives (...7z.001, ...7z.002, etc., or ...part1.rar, ...part2.rar, etc.). I'm currently considering a possible solution.

# Build Dependencies
Mormot2 - OpenSource RESTful ORM/SOA/MVC ToolBox for Delphi and FreePascal: https://github.com/synopse/mORMot2, which contains an excellent Delphi wrapper for 7z.dll

Baz's Debug Window - OpenSource: https://github.com/BazzaCuda/BazDebugWindow, an enhanced reworking of the original DebugWindow by GExperts

![image](https://github.com/user-attachments/assets/699114aa-15a6-4d0d-b261-8811f696096f)

