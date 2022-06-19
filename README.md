# legistext-NY

R scripts for basic text analysis of New York State legislation: bills introduced in the State Senate or Assembly.  
TidyBill.R reads a bill in .pdf format and converts it to a 'tidy' .csv format in which each text paragraph is accompanied 
by an outline tag usable to reference the paragraph and an optional list of keywords. Commonalities.R uses these keywords to 
identify paragraphs in a second bill that may be related to each paragraph in a first bill. Accompanying .pdf and .csv 
files illustrate capabilities of the Tidybill and Commonalities scripts. 

# Limitations

1. Bills in pdf format do not conform to any formal specification, so the scripts are using various heuristics to identify paragraph outline tags. They don't always get this right. The objective is only to provide a .csv file that can be made presentable with light editing. 

2. Scripts don't do any special handling of edits appearing in the pdf file. Text is included if it's visible, even though it may be shown with strikethrough markings in the pdf file. Such text is usually framed by square brackets [] in the .csv file and can be found and deleted pretty easily. 

3. The method for specifying the input files is crude. A Tkinter front-end would be nice. 
