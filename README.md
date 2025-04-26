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

** **Note**: Some three-word compounds are not yet captured. Future updates will include these.

## How to Use
Access the expanded dictionary through:
- Web interface: [Apte Dictionary Expanded](https://apte-dictionary.fly.dev/) - Search for Sanskrit compounds and find their headwords
- Direct lookup: Use the web interface to search for specific compounds and see where they appear in the dictionary
- Local installation:
  1. Clone this repository: `git clone https://github.com/sumanthegde/apte-uncompress.git`
  2. Checkout the deployment_scripts branch: `git checkout deployment_scripts`
  3. You must now have the dictionary data in `apteDir.nosync/output/` directory (contains `pagemarks.json`, `table_new.txt`, and `sharded/` folder)
  4. Run the server: `node serve.js --public . --data apteDir.nosync/output`
  5. Access the dictionary at `http://localhost:8080`

## Contributing
This project combines Haskell-based dictionary processing with a JavaScript-based web interface:

- **Haskell Backend** (master branch): Parses the original Apte dictionary, expands Sanskrit compounds, and generates sharded JSON files and lookup tables. The sharded output (found in the 'shard' branch under apteDir.nosync/output/sharded/) allows for efficient loading of dictionary entries.

- **JavaScript Frontend** (deployment_scripts branch): Provides a web interface that consumes the generated JSON data, offering search functionality and a clean presentation of dictionary entries.

To contribute:

### Setting up the development environment
1. Install Haskell Stack: `curl -sSL https://get.haskellstack.org/ | sh`
2. Clone this repository: `git clone https://github.com/sumanthegde/apte-uncompress.git`
3. Navigate to the project directory: `cd apte-uncompress`

### Generating dictionary data
1. Run `stack build` to compile the project
2. Run `stack run` to generate the processed dictionary files
   - This will create the expanded dictionary in `apteDir.nosync/output/`
   - The main output includes sharded JSON files and table_new.txt

### Project structure
- `src/` - Haskell source code for dictionary processing
- `app/` - Application entry point
- `apteDir.nosync/` - Dictionary data and output files

## Acknowledgments

This project builds upon the work of many individuals and organizations:

### Original Dictionary
- **[V S Apte's Sanskrit-English Dictionary](https://www.sanskrit-lexicon.uni-koeln.de/scans/AP90Scan/2020/web/webtc2/index.php)** - The original 1890 dictionary that forms the foundation of this project
- **[The Sanskrit Lexicon Project](https://www.sanskrit-lexicon.uni-koeln.de)** - For digitizing and making the dictionary accessible
- **[DSAL (Digital Dictionaries of South Asia)](https://dsal.uchicago.edu/dictionaries/apte/)** - For disambiguating many headwords

### Tools and Resources
- **[Ashtadhyayi.com](https://ashtadhyayi.com/)** - For Sanskrit linguistic resources and inspiration
- **[Sanscript.js](https://github.com/sanskrit/sanscript)** - For Sanskrit transliteration support


## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact
For questions, suggestions, or feedback:
- Open an [issue](https://github.com/sumanthegde/apte-uncompress/issues) on GitHub
- Contact via [email](mailto:sumant.sanskrit@gmail.com)
