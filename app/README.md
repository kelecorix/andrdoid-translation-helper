# Android Translation Helper

Small application that allows to keep shared translations spreadsheet and update correctly Android xml resource files and relevant translations. Useful when you want to cross-check available translations, work with collegues without technical background who are highly involved in process and very meticulous about grammar in different languages (marketers, pr managers, product managers)

1) generates csv with all strings and translations listed
2) reads `csv` file and updates xml files based on a key


## Installation

```bash
# stack install ath
```

## Usage

Importing (default mode)

```bash
$ ath -i myfiles/my-awesome-app-files/translations.csv -o work/projects-java/my-awesome-project/app/src/main/res
```
where `-i` input spreadsheet file location, `-o` xml files folder location

Exporting
```bash
ath -e -o myfiles/my-awesome-app-files/translations.csv -i work/projects-java/my-awesome-project/app/src/main/res
```
where `-e` indicates export mode, `-o` output file location, `-i` xml files floder location
