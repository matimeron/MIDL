Function PD_sdist_check, snum, frame= fnum, verify= ver, slit= sli, yoff= yof,$
	peak_cent = cen, range = ran, interactive = int, show = sho, wait = wai, $
	values = val, error = err, _extra= _e

;+
; NAME:
;		PD_SDIST_CHECK
; VERSION:
;		8.216
; PURPOSE:
;		Uses Z-integrated PD_GID_READ data to evaluate a correction to the 
;		PDSDIST value (S4-detector distance)
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = PD_SDIST_CHECK( SNUM [,keywords])
; INPUTS:
;	SNUM
;		A list of scan numbers, in any form recognizable by RANGE_PROC.  If more
;		than one scan is provided, PD_GID_READ will attempt patching in the
;		horizontal direction.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	FRAME
;		Accepts a frame number or list of frame numbers, in any form
;		recognizable by RANGE_PROC.  Valid only when a single scan is used,
;		automatically disabled for multiple scans.
;		Note that the default operation is always to use all frames.
;	/VERIFY
;		Switch.  If set, the existence of all frames to be processed is
;		verified and bad or missing frames are rejected.
;	SLIT
;		Numeric scalar, the value of the "eletronic slit" (around the center
;		pixel) in *pixels*.
;	YOFF
;		Numeric scalar, specifying Y_offset along the beam footprint.
;		Mandatory.
;	PEAK_CENT
;		Scalar, externally provided angle value (in degrees) for the center of
;		the peak (which is assumed to be present in the data).  Optional, if
;		not given a value is generated internally.
;	RANGE
;		Scalar.  If provided, the internal fitting is only performed within
;		the range [-RANGE,RANGE] around the maximum.
;	/INTERACTIVE
;		Switch.  If set, the user is given the opportunity to repeat the 
;		internal centroid evaluation, using different starting value(s).
;	/SHOW
;	 	Switch.  If set, the data and internally generated fits are displayed 
;	 	to the screen.  /SHOW is on by default, unless explicitly set to 0. 
;	 	Also, setting /INTERACTIVE activates /SHOW as well.
;	 WAIT
;	 	Sets waiting time (in seconds) for watching the plots.  Default is 1
;	 	second for /SHOW.  Not relevant for /INTERACTIVE.
;	 VALUES
;	 	Optional output, see below.
;	 ERROR
;	 	Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  See PD_GID_READ for more details.
; OUTPUTS:
;		Returns a calculated correction to the PDSDIST value (see routine
;		SPSCAN__DEFINE) in mm.
; OPTIONAL OUTPUT PARAMETERS:
; 	VALUES
; 		Returns a 2 element vector, containing the values [PDIST,PDSDIST] for 
; 		SNUM, with the calculated correction being applied to both.
;	ERROR
;		Returns an error estimate for the returned distance correction.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		All the PD_GID_READ restrictions.  Also, the data should present a 
; 		single well defined peak, else the results are unreliable.
; PROCEDURE:
;		Evaluates the data present in SNUM using both YOF and -YOF for Y 
;		offsets and estimates acorrection to PDSDIST using the shift (if
;		present) between the two data sets.
;		Calls PD_GID_READ.  Calls SCAN_FIELD_READ, SCAN_PD_FRAMES and 
;		SPEC_FILE_CHECK from SPEC.  Calls PEAK_FIT from BLIN.  Also calls 
;		DEFAULT, ISNUM and STREQ, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-DEC-2013 by Mati Meron.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	Spec_file_check, snum,/pil,par_con=['In_Rot','Out_Rot'],tol=1e-3,$
	nsc=nsc,lis=lis,_extra=_e
	if nsc gt 1 then wfnum = -1 $
	else wfnum = Scan_PD_frames(lis[0],fnum,verify=ver,/uni)
	cur = fildat.scan[lis[0]]

	cenfl = Isnum(cen)
	ranfl = Isnum(ran)
	intfl = keyword_set(int)
	shofl = intfl or keyword_set(Default(sho,1))
	wai = Default(wai,shofl-intfl)

	pdat = PD_GID_read($
		snum,frame=fnum,verify=ver,slit=sli,yoff=yof,/ang,/z_int,_extra=_e)
	if not cenfl then begin
		dum = max(pdat[1,*],loc)
		mloc = Default(cen,pdat[0,loc])
	endif else mloc = 1.*cen
	if ranfl then begin
		anran = where(pdat[0,*] ge mloc-ran and pdat[0,*] le mloc+ran)
		pdat = pdat[*,anran]
	endif
	repeat begin
		pcen = (Peak_fit($
			pdat,fun='lor',cen=mloc,back=3,err=err,sho=shofl,/quiet,den=4))[4]
		perr = err[4]
		if intfl then begin
			wait, 0.001
			que = ''
			read, que, prompt = '		OK (Y/N/Q)? '
			if Streq(que,'n',1) then begin
				read, mloc, prompt = '		Enter new center value: '
				done = 0
			endif else done = 1
		endif else done = 1
	endrep until done
	wait, wai

	ndat = PD_GID_read($
		snum,frame=fnum,verify=ver,slit=sli,yoff=-yof,/ang,/z_int,_extra=_e)
	if not cenfl then begin
		dum = max(ndat[1,*],loc)
		mloc = Default(cen,ndat[0,loc])
	endif else mloc = 1.*cen
	if ranfl then begin
		anran = where(ndat[0,*] ge mloc-ran and ndat[0,*] le mloc+ran)
		ndat = ndat[*,anran]
	endif
	repeat begin
		ncen = (Peak_fit($
			ndat,fun='lor',cen=mloc,back=3,err=err,sho=shofl,/quiet,den=4))[4]
		nerr = err[4]
		if intfl then begin
			wait, 0.001
			que = ''
			read, que, prompt = '		OK (Y/N/Q)? '
			if Streq(que,'n',1) then begin
				read, mloc, prompt = '		Enter new center value: '
				done = 0
			endif else done = 1
		endif else done = 1
	endrep until done
	wait = wai

	phi = !dtor*(pcen + ncen)/2
	dpsi = !dtor*(pcen - ncen)
	edpsi = !dtor*sqrt(perr^2 + nerr^2)
	l1 = cur.pdist - cur.pdsdist
	l2 = cur.pdsdist
	coe = l1*l2/(2*yof*sin(phi))
	res = 1.*round(coe*dpsi)
	err = coe*sqrt(edpsi^2 + (cur.pdpix/(2*cur.pdsdist))^2)
	val = res + [Scan_field_read(snum,'pdist',/con,tol=0.5),$
			Scan_field_read(snum,'pdsdist',/con,tol=0.5)]

	return, res
end