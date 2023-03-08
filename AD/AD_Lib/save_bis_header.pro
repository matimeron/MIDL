PRO Save_BIS_header, filename, _extra = _e

;+
; NAME:
;		SAVE_BIS_HEADER
; VERSION:
;		8.42
; PURPOSE:
;		Reads, modifies and saves a BIS format header.
; CATEGORY:
;		Data Input/Output.
; CALLING SEQUENCE:
;		Result = SAVE_BIS_HEADER, FILENAME
; INPUTS:
;	FILENAME
;		CCD data filename which serves as the source for the header.  If not
;		given will be querried for interactively.
; OPTIONAL INPUT PARAMETERS:
; 		None.
; KEYWORD PARAMETERS:
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to FILE_GET.
;		Not to be used directly.
; OUTPUTS:
; 		None, other than a file being saved.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
; 		BIS_HEADER.  Contains:
; 			BEXS	:	Status variable, set to 1 when previous data is defined.
; 			HEAD	:	Previously used SFRM header, character array.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that FILENAME must exist and be in BIS format.
; PROCEDURE:
;		Straightforward, Reads the header of the input file, modifies it to 
;		make it generic and saves the modified version in a new file. Calls 
;		READ_BIS_HEADER and STRARR_PUT. Calls FILE_GET and RANGE_PROC from MIDL.
; MODIFICATION HISTORY:
;		Created 30-APR-2015 by Mati Meron.
;-

	common bis_header, bexs, head

	on_error, 1

	bexs = 0
	head = Read_BIS_header(file=filename)
	hlen = n_elements(head)

	putloc = 8l
	blank = string(replicate(32b,linlen-putloc))
	zero = '0.000000'
	clear = Range_proc('11-22,25-26,32-36,38-41,61,62,64,86')

	Strarr_put, head, blank, putloc, line=clear
	Strarr_put, head, '-1', putloc, phrase='noverfl'
	Strarr_put, head, '0', putloc + 24*[1,2], phrase='noverfl'
	Strarr_put, head, zero, putloc, phrase='range'
	Strarr_put, head, zero, putloc, phrase='start'
	Strarr_put, head, zero, putloc, phrase='increme'
	Strarr_put, head, '1', putloc, phrase='number'
	Strarr_put, head, '0', putloc, phrase='nframes'
	Strarr_put, head, '0', putloc + 24*lindgen(3), phrase='nover64'
	Strarr_put, head, '4', putloc, phrase='npixelb'
	Strarr_put, head, '0', putloc + 36, phrase='npixelb'
	Strarr_put, head, 'UNKNOWN', putloc, phrase='correct'
	Strarr_put, head, 'UNKNOWN', putloc, phrase='warpfil'		
	Strarr_put, head, 'Pilatus         ', putloc, phrase='dettype'
	Strarr_put, head, '58.139534', putloc + 21, phrase='dettype'
	Strarr_put, head, '    0   ', putloc + 24, phrase='nexp'
	Strarr_put, head, 'NONE', putloc, phrase='dark'
	bexs = 1

	wnam= File_get($
	'bis_file_header',default='sfrm',/write,/over,stat=stat,_extra=_e)
	if stat then begin
		linlen = 80l
		rform = strcompress('('+string(hlen)+'a'+string(linlen)+')',/rem)
		openw, hdun, wnam, /get_lun
		printf, hdun, head, format = rform
		free_lun, hdun
	endif else message, 'File not written!', /cont

	return
end