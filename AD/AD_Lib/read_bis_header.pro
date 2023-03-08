Function Read_BIS_header, filename = fnam, blknum = blkn, _extra = _e

;+
; NAME:
;		READ_BIS_HEADER
; VERSION:
;		8.42
; PURPOSE:
;		Reads BIS format header.
; CATEGORY:
;		Data Input/Output.
; CALLING SEQUENCE:
;		Result = READ_BIS_HEADER( FILENAME = FNAM [, keywords])
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
;	FILENAME
;		CCD data filename.  If not given will be querried for interactively.
;		Serves as both input and output paramater, as on return it contains
;		the full filename used.
;	BLKNUM
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to FILE_GET.
;		Not to be used directly.
; OUTPUTS:
; 		Returns the file header as a string array.
; OPTIONAL OUTPUT PARAMETERS:
;	BLKNUM
;		The number of header blocks present.
; COMMON BLOCKS:
; 		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data file must exist and be in BIS format.
; PROCEDURE:
;		Straightforward, relies on the BIS data definitions (see there).
;		Calls FILE_GET, STRMATCH_MM and STRPARSE_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-APR-2015 by Mati Meron, through surgery on READ_BIS.
;-

	on_error, 1

	fnam = File_get(fnam,stat=stat,_extra=_e)
	if stat then begin
		openr, apun, fnam, /get_lun
		hcheck = 'hdrblks'
		blklen = 512l
		linlen = 80l
		hlen = 3
		head = strarr(hlen)
		rform = strcompress('('+string(hlen)+'a'+string(linlen)+')',/rem)
		readf, apun, head, form = rform
		loc = Strmatch_mm(hcheck,head,strlen(hcheck))
		if loc ge 0 then begin
			point_lun, apun, 0
			dum = Strparse_mm(head[loc],' :	',lis)
			blkn = fix(lis[dum])
		endif else begin
			free_lun, apun
			message, 'File corrupted, exiting!'
		endelse

		hlen = blkn*blklen/linlen
		head = strarr(hlen)
		rform = strcompress('('+string(hlen)+'a'+string(linlen)+')',/rem)
		readf, apun, head, form = rform
		free_lun, apun
	endif else message, 'Bad or missing file!'

	return, head
end