Pro Load_sources, fname

;+
; NAME:
;		LOAD_SOURCES
; PURPOSE:
;		Loads X-ray sources parameters from a provided data file and
;		establishes the common block SXR_STUFF (can also be established by the
;		procedure LOAD_ABS_COEFFS).
; CATEGORY:
;		X-ray calculations
; CALLING SEQUENCE:
;		LOAD_SOURCES [, FNAME]
; INPUTS:
;	FNAME
;		Name of data file.  Default is XRAY_SOURCES.  Default extension is DAT.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		SXR_STUFF.  Currently contains the source table and the mass absorption
;		coefficient table, both defined as arrays of structures.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Reads the data and creates an array of structures of type SYLS, one
;		structure for each machine.  Each SYLS structure containes, in order:
;		MACHINE	-  Machine's name.
;		ENERGY	-  Machine's energy (GeV).
;		CURRENT	-  Machine's current (Amp).
;		NLINES	-  Number of devices listed for this machine.
;		LINE	-  An array of structures of type BLINE, one for each device.
;			Each BLINE structure contains the following:
;			TYPE  -	Device type.  1 for BM, 2 for wiggler, 3 for undulator.
;			NAME  -	Name of device.
;			MRAD  -	Horizontal beam spread (milirads).
;			ECRIT - Critical energy (eV).
;			SIGX  -	Horizontal source spread parameter.
;			SIGY  -	Vertical source spread parameter.
;			KVAL  -	Value of K (wherever relevant).
;			POLES -	Number of poles (wherever relevant).
;
;		Uses calls to DEFAULT, STRPARSE_MM, and STRMATCH_MM in MIDL.
; MODIFICATION HISTORY:
;		Created 20-MARCH-1993 by Mati Meron.
;-

	common sxr_stuff, sorlist, abctab, enun, denun, curbeam
	dmax = 16
	blank = {syls, machine: '', energy: 0., current: 0., nlines: 0, $
		line: replicate({bline, type: 1, name: '',mrad: 1., $
		ecrit: 0., sigx: 0., sigy: 0., kval: 0., poles: 1}, dmax)}
	kwords = ['begin', 'end']

	on_ioerror, file_no_good
	fname = Default(fname,'xray_sources')
	fname=file_get(fname,filt='dat',path=getenv('sruff_data'),/pick,stat=stat)
	if stat then openr, datun,fname, /get_lun else message,'Cannot find file!'

	nso = -1
	datfl = 0
	line = ''
	on_ioerror, data_no_good
	while not eof(datun) do begin
		readf, datun, line
		if line ne '' then begin
			il = Strparse_mm(line,'  ',list)
			mnum = Strmatch_mm(list(0),kwords,3)
			case (datfl + mnum + 1) of
				1	:	begin
							nso = nso + 1
							if nso eq 0 then slist= blank $
							else slist= [slist,blank]
							slist(nso).machine  = list(1)
							slist(nso).energy   = list(2)
							slist(nso).current  = list(3)
							if il gt 3 then begin
								if il ge 6 then begin
									defvals = float(list(4:6))
									deffl = 1
								endif else message,'Give all defaults or none!'
							endif else deffl = 0
							datfl = 3
							nli = -1
						end
				2	:	message, 'END without BEGIN, aborting!'
				3	:	begin
							nli = nli + 1
							if nli eq dmax then message, $
							'Too many !@#$%^& beamlines!'
							slist(nso).line(nli).type = list(0)
							slist(nso).line(nli).name = list(1)
							if slist(nso).line(nli).type gt 1 then begin
								for i = 2, 7 do $
								slist(nso).line(nli).(i) = list(i)
							endif else begin
								if il ge 2 then $
								slist(nso).line(nli).mrad = list(2)
								for i = 3, (il < 5) do $
								slist(nso).line(nli).(i)=list(i)
								if il lt 5 then begin
									if deffl then for i = 1 + (2 > il), 5 do $
									slist(nso).line(nli).(i) = defvals(i-3) $
									else message,'Some parameters are missing!'
								endif
							endelse
						end
				4	:	message, 'BEGIN without END, aborting!'
				5	:	begin
							slist(nso).nlines = nli
							datfl = 0
						end
				else:
			endcase
		endif
	endwhile
	if datfl ne 0 then message, 'Missing END, aborting!'

	free_lun, datun
	return

	data_no_good:
	free_lun, datun
	file_no_good:
	print, !err_string
	return
end