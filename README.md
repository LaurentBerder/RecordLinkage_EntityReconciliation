# RecordLinkage_EntityReconciliation
Finding approximate duplicates between two sets of people databases, based on First Name/Last Name (with fuzzy logic), adresses, phone numbers, etc.


The industrial request was to link (if possible) people from their existing database to people in external documents.
The external documents contained between 1 000 and 3 000 lines, while the internal database was in the order of 3 000 000 to 10 000 000 lines.



The fields available for linking individuals were First Name, Last Name, Adress, Zipcode, Phone Number, Email. Of course, all of these fields are very prone to fluctuations in spelling or typing, which required the use of fuzzy logic in the searches.
