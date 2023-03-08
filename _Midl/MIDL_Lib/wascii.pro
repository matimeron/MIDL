Pro Wascii, arr, filnam, title= tit, header= hed, format= form, verbose= vrb, $
	_extra = _e

;+
; NAME:
;		WASCII
; VERSION:
;		8.05
; PURPOSE:
;		Writes an array into an ASCII file.
; CATEGORY:
;		Input/Output.
; CALLING SEQUENCE:
;		WASCII, ARR, FILNAM [, keywords]
; INPUTS:
;	ARR
;		Array, arbitrary, no more than 16 columns.
;	FILNAM
;		Char. value, the name of the data file.  On VMS, Default extension
;		is '.DAT'.
;
;		Note:  	When a file name is provided, an interactive GUI will open to
;				aid the user.  This is the default action.  It can be overriden
;				by using the keyword /AUTO which is passed by _EXTRA to
;				FILE_GET.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	TITLE
;		Character string, used as the title of the written table.  Default is 
;		no title.
;
;		Note:	In previous versions the title was passed to TABULATE through
;				_EXTRA.  This has been changed to avoid conflicts with FILE_GET.
;	HEADER
;		Character array, optional, if provided, the entries are used as column
;		titles.  Null string entries translate to default column headings of
;		TABULATE.  If not provided, no column headers appear in the file.
;	FORMAT
;		Character string translating to a valid format.  If provided will apply
;		to all the columns.  If not provided. the default formats of TABULATE
;		are used.
;	/VERBOSE
;		Switch.  If set, the full name of the generated file is printed to the
;		screen.
;	_EXTRA
;		A formal keyword, used to transfer keywords to FILE_GET and TABULATE.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The number of columns (first index of array) is limited.  See
;		restrictions in TABULATE.
; PROCEDURE:
;		Straightforward, using TABULATE from MIDL.  Also calls DEFAULT,
;		FILE_GET and WHEREINSTRUCT from MIDL.
; MODIFICATION HISTORY:
;		Created 30-MAY-1996 by Mati Meron.
;		Modified 10-JUL-1997 by Mati Meron.  Name changed from WRITE_ASCII to
;		WASCII, to avoid potential conflicts with future IDL releases.
;		Checked for operation under Windows, 30-JAN-2001, by Mati Meron.
;		Modified 10-AUG-2003 by Mati Meron.  Added interactive filename
;		selection.
;		Modified 20-NOV-2005 by Mati Meron.  Internal changes only.
;		Modified 20-JAN-2008 by Mati Meron.  Internal changes only.
;		Modified 1-MAR-2011 by Mati Meron.  Added keyword VERBOSE.
;		Modified 5-JUN-2011 by Mati Meron.  Added keyword TITLE.
;-

	on_error, 1
	siz= size(arr)
	if siz[0] eq 1 or siz[0] eq 2 then nc = siz[1] else $
	message, 'not a matrix!'
	if nc gt 16 then message, 'At most eight columns are allowed!'

	dum = Wherinstruct('call',_e)
	if dum ge 0 then begin
		lc = _e.(dum)
		_e.(dum) = _e.(dum) + 1
	endif else lc = 1
	ofil = File_get(filnam,stat=stat,/write,/over,def='txt',call=lc,_extra=_e)
	if not stat then message, 'Missing filename!'

	ofor = Default(form,'',/dtype)
	nf = n_elements(ofor)
	if nf lt nc then ofor = [ofor,replicate(ofor[nf-1],nc-nf)]
	nohed = n_elements(hed) eq 0

	com = 'Tabulate, arr[0,*],/auto,call=2'
	for i = 1, nc - 1 do com = com + ',arr[' + string(i,form="(i2)") +',*]'
	com = com + $
	', title= tit, head= hed, nohead= nohed, form= ofor, file= ofil, _extra =_e'
	idum = execute(com)
	if keyword_set(vrb) then print, '	Saved ' + ofil

	return
end