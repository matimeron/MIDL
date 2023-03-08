Function File_last, fnam, path= pat, name= nam, extension= ext, prevnum= pre, $
	number = num

;+
; NAME:
;		FILE_LAST
; VERSION:
;		7.12
; PURPOSE:
;		Finds the last (newest) file in a directory.
; CATEGORY:
;		Utility.
; CALLING SEQUENCE:
;		Result = FILE_LAST( FNAM [, keywords])
; INPUTS:
;	FNAM
;		Optional string scalar, representing a full or partial file name.  If 
;		given, must be of character type.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	PATH
;		String scalar representing file path.
;	NAME
;		String scalar representing (net) file name.
;	EXTENSION
;		String scalar representing file extension.
;
;		Note:	1)	If FNAME is given and includes file path, name or extension,
;					the corresponding keywords above are ignored.
;				2)	Path, name andextension may include wild cards.
;				3)	If path is not provided (by either FNAM or PATH), the path
;					to the current directory is used.
;				4)	If name is not provided, the wildcard '*' is used
;				5	If extension is not provided, the wildcard '*' is used.
;	PREVNUM
;		Integer scalar (defaults to 0).  If the number of files found, 
;		satisfying the conditions, is less or equal to PREVNUM, the routine
;		abandons processing and exists.  PREVNUM exists for internal programming
;		purposes, to avoid reprocessing of file lists that didn't change since
;		the previous call.
;	NUMBER
;		Optional output, see below.
; OUTPUTS:
;		Returns the full filename of the last written file satisfying the 
;		conditions (Path, name, extension).  If no such file exists or if the
;		number of files in existance is smaller than PREVNUM (see above), 
;		FILE_LAST returns a null string.
; OPTIONAL OUTPUT PARAMETERS:
;	NUMBER
;		Returns the number of files satisfying the conditions.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		All inputs, if present, must be of type STRING.  Missing inputs are 
;		taken as null strings.
; PROCEDURE:
;		Straightforward.  Calls DEFAULT and FNAMPARSE from MIDL.  Also calls 
;		SDEP from imports.
; MODIFICATION HISTORY:
;		Created 25-NOV-2009 by Mati Meron.
;-

	on_error, 1
	ds = sdep(/ds)
	ts = '.'
	wild = '*'

	cd, cur = cpat
	wnam = Fnamparse(fnam,path=wpat,ext=wext)
	if wpat eq '' then begin
		cd, cur = cpat
		wpat = Default(pat,cpat)
		if strpos(wpat,ds,/reverse_search) lt (strlen(wpat) - 1) $
		then wpat = wpat + ds
	endif
	if wnam eq '' then wnam = Default(nam,wild)
	if wext eq '' then begin
		wext = Default(ext,wild)
		if strpos(wext,ts) ne 0 then wext = ts + wext
	endif

	list = file_search(wpat+wnam+wext,count=num)
	if num gt Default(pre,0) then begin
		linfo = file_info(list)
		lname = linfo.name
		ltime = linfo.mtime
		s = reverse(sort(ltime))
		rnam = lname[s[0]]
	endif else rnam = ''

	return, rnam
end	