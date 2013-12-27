7-Zip Extra 9.20
----------------

7-Zip Extra is package of extra modules of 7-Zip. 

7-Zip Copyright (C) 1999-2010 Igor Pavlov.

This package contains the following files:

7za.dll   - library for working with 7z archives.
7zxa.dll  - library for extracting from 7z archives.
7zS.sfx      - SFX module for installers
7zSD.sfx     - SFX module for installers (uses msvcrt.dll)
7zS2.sfx     - small SFX module (GUI version) (uses msvcrt.dll)
7zS2con.sfx  - small SFX module (Console version) (uses msvcrt.dll)
7zr.exe   - reduced version of console program 7za.ex
copying.txt  - GNU LGPL
readme.txt   - This file
Far\      - Plugin for FAR manager
Installer\  - Files to compress installers
x64\      - DLLs for x64


Features of 7za.dll: 
  - Supporting 7z format;
  - Built encoders: LZMA, LZMA2, PPMD, BCJ, BCJ2, COPY, AES-256 Encryption.
  - Built decoders: LZMA, LZMA2, PPMD, BCJ, BCJ2, COPY, AES-256 Encryption, BZip2, Deflate.
7zxa.dll supports only decoding from .7z archives.
    

7-Zip is free software. Read file License.txt for more infomation about license.

Source code of 7za.dll and 7-Zip (including all interfaces 
and small example application) can be found at

http://www.7-zip.org/

7za.dll and 7zxa.dll can work in Windows 95/98/ME/NT/2000/XP/2003. 
7za.dll and 7zxa.dll work via COM interfaces
But these DLLs don't use standard COM interfaces for objects creating.


---
End of document
