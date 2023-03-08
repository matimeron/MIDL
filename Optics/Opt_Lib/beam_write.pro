Pro Beam_write, beam, und_length = udl, file = fnam, verbose = verb

;+
; NAME:
;		BEAM_WRITE
; VERSION:
;		8.21
; PURPOSE:
;		Writes out a data file representing a beam structure of type OPBEAM.
; CATEGORY:
;		Optical calculations Input/Output.
; CALLING SEQUENCE:
;		BEAM_WRITE, BEAM [, keywords]
; INPUTS:
;	BEAM
;		Structure of type OPBEAM.  See OPBEAM__DEFINE for details.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	UND_LENGTH
;		Value of undulator length (meter).  If given, radiative corrections
;		are subtracted from the present linear and angular sizes.
;	FILE
;		The name of the file to write.  If not given, it is quereed for,
;		interactively.
;	/VERBOSE
;		Switch.  If set, the full name of the generated file is printed to the
;		screen.
; OUTPUTS:
;		None other than the generated file.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		Block BEAM_STUFF, as defined in INIT_OPT.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls BEAM_FIELDS.  Calls FILE_GET, ISNUM, STREQ and
;		VERIFY_STRUCT from MIDL.
; MODIFICATION HISTORY:
;		Created 10-DEC-2006 by Mati Meron.
;		Modified 25-AUG-2013 by Mati Meron.  Bug fix.
;-

	common beam_stuff, exs, npomax, mmicr, mwlen
	on_error, 1

	if not Verify_struct(beam,'opbeam') then message, $
	'BEAM must be a scalar structure of type "OPBEAM"!'
	if beam.npoints le 2 then message, 'Insufficient for writing!'

	if Isnum(udl) then begin
		clen = udl/(2*!pi)
		corr = beam.wavl*[clen,1/clen]
	endif else corr = 0

	xfl = beam.set and 1
	yfl = beam.set/2 and 1

	if xfl then begin
		xsrsq = beam.xsec[0].bpars[1:2]
		xsr = sqrt(xsrsq - corr)/mmicr
	endif

	if yfl then begin
		ysrsq = beam.ysec[0].bpars[1:2]
		ysr = sqrt(ysrsq - corr)/mmicr
	endif

	Beam_fields, beam,ele= ele,loc= loc,par= par,cod= cod,els= els,elnam= enm

	wnam = File_get(fnam,default='dat',/write,/over,stat=stat,_extra=_e)
	if stat then begin
		openw, unit, wnam, /get_lun
		printf, unit, 'Begin_source'
		printf, unit, '	wavelength	', string(beam.wavl/mwlen,form='(g13.7)')
		if xfl then printf, unit, '	x_source	',string(xsr,form='(2g13.7)')
		if yfl then printf, unit, '	y_source	',string(ysr,form='(2g13.7)')
		printf, unit, '	sigma		', 0
		if Isnum(udl) then printf, unit, '	und_len	', udl
		if not Streq(beam.name,'') then  $
		printf, unit, '	name		', beam.name
		if not Streq(beam.elname[0],'') then $
		printf, unit, '	sorname		', beam.elname[0]
		printf, unit, 'End_source'
		printf, unit
		printf, unit, 'Begin_beam'
		for i = 1l, n_elements(enm)-1 do printf, unit, $
		enm[i],ele[i],loc[i],par[i],cod[i],els[i], $
		form = '(t5,a,t17,a,t25,f8.3,g13.7,i4,i4)'
		printf, unit, 'End_beam'
		free_lun, unit
		if keyword_set(verb) then begin
			print
			print, '	Saved ' + wnam
			print
		endif

	endif else message, 'File not written!', /cont

	return
end