# Invirus

A simple virus-scanning application for Linux.


## Build and Run

Just run:

`stack build`

and then (without installation):

`stack exec Invirus-exe`

but if you decide to install it using:

`stack install`

then the “stack exec” part is no longer needed to run the program.


## How It Works

When the program is launched, a prompt appears, asking you to specify the path on the system from which the recursive file scan should begin. After calculating the hash (the type of hash is determined by the settings in the config.ini file), the application checks whether this file already exists in the local database. If there is no such entry in the database, a request is sent to VirusVault to retrieve information. The result is displayed in the console. Afterward, a record is added to the database, which serves as a history and, at the same time, a safeguard in case the same malware reappears. In addition, there are currently three options available upon malware detection: 
- Remove the file
- Print the file’s contents
- Continue

If you select any option other than “Continue,” these three options will reappear so you can resume working from the same point.


## Notes

- There are 2 attempts to send a request to VirusVault
- You can choose between md5, sha1, and sha256 in the config.ini file. Selecting any other option will result in md5 being used automatically, since VirusVault uses only these three types
- There is a total of 1.5 seconds between requests to prevent flooding the site with too many requests