- (EASY) Option to add entries via Cli (rather than a temporary file).
  This has use case for automated journaling
- Ability to specify another time for the journal entry (Ex. `yesterday: Did something important.`)
- Templates. Specify a template format for a journal entry
- Filter on time as well as tags
- Filter on Title (takes the first line of an entry and interprets that as a Title)
- Filter on entry content (does it contain a certain phrase or regex)
- Exclusion filter
- Modification commands (edit, delete, etc.) that respect a searh.
  I.e., edit only entries searched for from the given filters.
- (EASY & UNESSENTIAL) A tip message at the top of the file where users submit there entry that gets automatically removed
  when saved.
  It would explain what the purpose of the opened buffer is for. (Ex. "Enter your journal entry below :")

Bugs / Issues :
  - Viewing entries in a buffer `view tag1 tag2 --editor` doesn't display time in correct timezone (it's just in UTC)

Completed :
- (EASY) Hide date and tags by default, and show with `--verbose / -v`. Instead, just print a line
  break "---------------------------"
