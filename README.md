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
  - To search the headword, type it in Devanagari or SLP1
  - To search the meaning text, prefix your string with a slash (/). For example, "/elephant" (without quotes) gives you all entries which have "elephant" in their meaning texts.
- Local installation:
  1. Clone this repository: `git clone https://github.com/sumanthegde/apte-uncompress.git`
  4. From the project root, run the server: `node web/server/serve.js --public web/public --data data/output`
  5. Access the dictionary at `http://localhost:8080`
- Programmatic access: Access the dictionary data programmatically via CDN (see [JSON Data Access](#json-data-access) section below)


## Contributing
This project combines Haskell-based dictionary processing with a JavaScript-based web interface:

- **Haskell Backend** (in `haskell/` directory): Parses the original Apte dictionary, expands Sanskrit compounds, and generates sharded JSON files and lookup tables. The sharded output (found in `data/output/sharded/`) allows for efficient loading of dictionary entries.

- **JavaScript Frontend** (in `web/` directory): Provides a web interface that consumes the generated JSON data, offering search functionality and a clean presentation of dictionary entries.

To contribute:

### Setting up the development environment
1. Install Haskell Stack: `curl -sSL https://get.haskellstack.org/ | sh`
2. Clone this repository: `git clone https://github.com/sumanthegde/apte-uncompress.git`
3. Navigate to the project directory: `cd apte-uncompress`

### Generating dictionary data
1. Navigate to the Haskell directory: `cd haskell`
2. Run `stack build` to compile the project
3. Run `stack run` to generate the processed dictionary files
   - This will create the expanded dictionary in `data/output/`
   - The main output includes sharded JSON files and table_new.txt

### Project structure
- `haskell/` - Haskell code for dictionary processing
  - `src/` - Haskell source code
  - `app/` - Application entry point
  - `test/` - Test files
  - `apteDir.nosync/` - Dictionary data and output files
- `web/` - JavaScript web interface
  - `public/` - Static web assets (HTML, CSS, client-side JS)
  - `server/` - Node.js server code
- `data/` - Dictionary data (symlink to haskell/apteDir.nosync)
- `kosha-flat/` - Contains JSON files with one entry per expanded dictionary entry, making it easy to consume individual entries directly
- `kosha-nested/` - Contains JSON files that maintain the original nested structure of the dictionary, with one file per headword

### JSON Data Access
For programmatic access to the dictionary data, we recommend using the flat structure format, which provides one JSON file per expanded entry. You can access these files via CDN using the following pattern:

```
https://cdn.jsdelivr.net/gh/sumanthegde/apte-uncompress@main/kosha-flat/{entry}.json
```

Example: `https://cdn.jsdelivr.net/gh/sumanthegde/apte-uncompress@main/kosha-flat/राम.json`

**Note on Nested Structure**: The project also maintains a nested structure format (in the `kosha-nested/` directory) which is primarily used internally by the frontend. This structure is considered an implementation detail and may change in the future for performance optimizations. External consumers should use the flat structure format shown above for stability.

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
