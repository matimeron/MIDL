Pro Weps_mm, fname, verbose= vrb, _extra = _e

;+
; NAME:
;		WEPS_MM
; VERSION:
;		8.335
; PURPOSE:
;		Generating EPS files.
; CATEGORY:
;		I/O
; CALLING SEQUENCE:
;		WEPS_MM, [FNAME] [, keywords]
; INPUTS:
;	FNAME
;		Character variable or constant, translating to a file name.  In standard
;		operation the user will be selecting or modifying FNAME through
;		DIALOG_PICKFILE.  This can be overriden by using the keyword /AUTO
;		which is passed by _EXTRA to FILE_GET.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/VERBOSE
;		Switch.  If set, the full name of the generated file is printed to the
;		screen.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to the DEVICE
;		routine.  See available keywords there.  Not to be used directly.
; OUTPUTS:
;		None, other than the file saved.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, calls FILE_GET from MIDL.  Interactive mode, unless the
;		keyword /AUTO (see FILE_GET) is invoked.
; MODIFICATION HISTORY:
;		Created 10-DEC-2014 by Mati Meron.
;		Modified 15-DEC-2014 by Mati Meron.  Bug fix.
;-

	on_error, 1

	wname = File_get(fname,def='eps',/write,/over,stat=stat,_extra=_e)
	if stat then begin
		Plvar_keep, action = 'save'
		img = TVRD(true=1)
		oxsiz = !d.x_size/!d.x_px_cm
		oysiz = !d.y_size/!d.y_px_cm
		odname = !d.name
		set_plot, 'PS'
		device, prev = 2, /encapsulated, bits_per_pixel = 8, /color, $
		xsize = oxsiz, ysize = oysiz, file = wname, _extra=_e
		tvlct, red, gre, blu, /get
		lcol = lindgen(!d.table_size)
		tvlct, lcol, lcol, lcol
		tv, img, true=1
		tvlct, red, gre, blu
		device, /close
		set_plot, odname
		Plvar_keep, action = 'restore'
		if keyword_set(vrb) then begin
			print
			print, '	Saved ' + wname
			print
		endif
	endif else message, 'File not saved!', /cont

	return
end