# apte-uncompress
Generate searchable lists of Sanskrit compounds from Apte's dictionary for easier lookup.

## Introduction
[V S Apte's Sanskrit-English Dictionary](https://www.sanskrit-lexicon.uni-koeln.de/scans/AP90Scan/2020/web/webtc2/index.php) (1890) 
is an invaluable resource for students of Sanskrit. 
However, its terse structure, where compounds (samāsa) are nested under 
their first component (pūrvapada), makes it difficult to search 
for specific compounds using standard search tools.

This project parses the dictionary and generates a comprehensive list 
of nearly all** compounds, annotated with their respective headwords. 
This allows users to locate the proper headword in the dictionary 
when unsure of a compound’s structure.

** **Note**: Currently, three-word compounds are not listed. Future updates will include these.

## How to Use
Access the list of words through the following options:
- View the [Google Spreadsheet](https://docs.google.com/spreadsheets/d/e/2PACX-1vSflaFYhAA77On2ecU3jkEzFbsGhUaP-E8rzc2HCYissRyeUezjQOB16uRn8BD7Dg3aXdE4sTtPhwW2/pubhtml).
- Download and open [table.txt](apteDir.nosync/output/table.txt).

## Contributing
If you want to generate the [table.txt](apteDir.nosync/output/table.txt) using the code, follow these steps:
1. Install stack: `stack curl -sSL https://get.haskellstack.org/ | sh`
2. In the project directory, run `stack run`.
   This generates [table.txt](apteDir.nosync/output/table.txt).

## Acknowledgments

- **[V S Apte's Sanskrit-English Dictionary](https://www.sanskrit-lexicon.uni-koeln.de/scans/AP90Scan/2020/web/webtc2/index.php)**
- **[The Sanskrit Lexicon Project](https://www.sanskrit-lexicon.uni-koeln.de)**
- **[Ashtadhyayi.com](https://ashtadhyayi.com/)**
- **[DSAL](https://dsal.uchicago.edu/dictionaries/apte/)**

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact
For questions or suggestions, please open an issue or contact via [email](mailto:sumantpes+apte@gmail.com).
