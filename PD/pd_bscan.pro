Pro PD_Bscan, snum, fnum, slit = sli, back_ang = bag, tilt = tau, $
	center = cnt, locate = lct, tolerance = tol, rcenter = ct, $
	correct_q = coq, pxval = pxv, qyval = qyv, qzval = qzv, $
	result = res, raw = raw, _extra = _e

;+
; NAME:
;		PD_BSCAN
; VERSION:
;		8.475
; PURPOSE:
;		Reads a list of Pilatus scans, extracts signal and background data and
;		generates net signal, for diffuse scattering.
; CATEGORY:
;		Surface specific
; CALLING SEQUENCE:
;		PD_BSCAN ( SNUM [, keywords])
; INPUTS:
;	SNUM
;		A list of scan numbers, in any form recognizable by RANGE_PROC.
; OPTIONAL INPUT PARAMETERS:
; 	FNUM
;		Frame number.  Allowed only when SNUM stands for a single scan and then
;		FNUM can only stand for a single frame.  The default is to use all the 
;		frames.
; KEYWORD PARAMETERS:
;	SLIT
;		Numerical scalar, the electronic slit dimension, in pixels.  Mandatory.
;	BACK_ANG
;		Numerical scalar, the "sideways" offset angle used to evaluate
;		background, in degrees.  Mandatory.
;	TILT
;		Optional detector tilt angle (couterclockwise) in degrees.
;	CENTER
;		Numerical scalar, location of the horizontal center of 	| At most one
;		the reflectivity peak, in pixels.						| of these two
;	/LOCATE														| may be used.
;		Switch,  If set, the center is located automatically.	| If neither is,
;		If the center such located is beyond the approved range	| the nominal
;		around the nominal center, it is rejected and the 		| center will
;		nominal center is used instead.							| be used.
;	TOLERANCE
;		Numeric scalar, value of the acceptable horizontal shift of the
;		reflectivity peak from the nominal center, in pixels, when /LOCATE is
;		used.  If not given, defaults to 6 pixels (about 1mm).
;	RCENTER
;		Optional output, see below.
;	/CORRECT_Q
;		Switch.  If set, the signal-backgroud subtraction is done for constant
;		Q instead of constant angle.
;	/PXVAL													|
;		Switch.  If set, data is displayed (and returned	|	At most one of
;		in RESULT) as a function of detector pixels.		|	these 3 may be
;	/QYVAL													|	set.  If none
;		Switch.  If set, data is displayed (and returned	|	is, data is
;		in RESULT) as a function of Q_y.					|	displayed as
;	/QZVAL													|	function of the
;		Switch.  If set, data is displayed (and returned	|	angle Beta.
;		in RESULT) as a function of Q_z.					|	
;	RESULT
;		Optional output, see below.
;	RAW
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  This includes (among others) the following keywords,
;		affecting data readout:
;
;			/BAD
;				Switch.  If set, faulty (very high count rate) pixels are
;				removed from the data.
;			/NORM
;				Switch.  If set, the data is normalized to monitor counts.
;
;		See SCAN_PD_READ for more details.
; OUTPUTS:
;		Only screen output directly, showing integrated (horizontally) net
;		signal as a function of Beta, Q_y or Q_z.  Other outputs through 
;		Optional Outputs.
; OPTIONAL OUTPUT PARAMETERS:
;	RCENTER
;		Returns the location (in pixels) used for horizontal coordinate of the
;		reflectivity peak.
;	RESULT
;		Returns the integrated horizontally net signal as a function of Q_y, in
;		the standard 1D scan format (3 columns).
;	RAW
;		Returns the sum of all the frames as a 2D image.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward, sums separately all the frames, extracts signal and
;		background data and generates net signal, optionally corrected for
;		change of Q.  Reads the data using parameters stored in the FILDAT
;		structure in the common block.  Calls IMG_INT, SCAN_CLEAN,
;		SCAN_PD_CENTER, SCAN_PD_FAROFF, SCAN_PD_FRAMES, SCAN_PD_LCOO, 
;		SCAN_PD_READ, SCAN_SHOW, SCAN_PD_TOL and SPEC_FILE_CHECK.  Calls QVEC
;		from SURF_LIB.  Also calls DEFAULT, HOW_MANY, ISNUM, MAKE_GRID, ONE_OF
;		and RANGE_COMP, from MIDL.
; MODIFICATION HISTORY:
;		Created 5-AUG-2008 by Mati Meron, as PD_DIFFUSE_ALT
;		Modified 15-AUG-2008 by Mati Meron.  Internal changes.
;		Modified 25-AUG-2008 by Mati Meron.  Internal changes.
;		Renamed PD_BSCAN on 25-JUN-2009 by Mati Meron.
;		Modified 5-FEB-2010 by Mati Meron.  Internal changes.
;		Modified 20-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 20-FEB-2011 by Mati Meron.  Added far detector support.
;		Modified 25-MAR-2011 by Mati Meron.  Updated error calculations for APEX
;		Modified 15-JUN-2011 by Mati Meron.  Added single frame option.
;		Modified 1-AUG-2011 by Mati Meron.  Added keyword QYVAL.  Changed 
;		default display to counts as a function of Beta.
;		Modified 25-AUG-2016 by Mati Meron.  Internal changes, Pilatus1M related
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	Spec_file_check, snum, /pildet, par_const = ['In_rot','Out_rot','Det_th'], $
	field_const = ['Pdist','Pdsdist'], nsc = nsc, lis = slis, _extra = _e
	if n_elements(fnum) ne 0 then begin
		if nsc eq 1 then wfnum = Scan_PD_frames(snum,fnum,ver=ver) $
		else message, "Can't select frames with multiple scans!"
		if n_elements(wfnum) gt 1 then message, $
		'Only single frames allowed when selecting!'
	endif

	cur = fildat.scan[slis[0]]
	apfl = (cur.pdstat - 3)/2
	cent = Scan_PD_center(snum,dim=dim)
	alp = (fildat.scan[slis[0]].angs)[0]
	if cur.pdfar then begin
		cent = cent - Scan_PD_faroff(slis[0],wfnum,/ave)
		bet = 0.
	endif else bet = (fildat.scan[slis[0]].angs)[1]
	lam = fildat.scan[slis[0]].lambda
	sbet = Scan_PD_lcoo(slis[0],wfnum,dir='ver',/ang)

	if n_elements(sli) gt 0 then hs = round(sli[0])/2 $
	else message, 'Missing slit size!'
	if Isnum(bag) then begin
		hoff = round(bag*!dtor*cur.pdist/cur.pdpix)
		if hoff le 2*hs then begin
			bagm = cur.pdpix*(2*hs[0]+1)/(!dtor*cur.pdist)
			message, 'Minimal offset for current slit is ' + $
			string(bagm,form='(f6.3)') + ' degrees!'
		endif
	endif else message, 'Background angle needed!'

	for i = 0l, nsc-1 do begin
		tem = Scan_PD_read(slis[i],wfnum,/jimg,/bad,tit=dtit,_extra=_e)
		if i eq 0 then raw = tem else raw = raw + tem
	endfor
	dtit = strmid(dtit,0,strpos(dtit,'_')) +' S# ' + Range_comp(slis)

	if Isnum(tau) then begin
		wtau = !dtor*tau
		xz = Make_grid([[0,dims[0]-1],[0,dims[1]-1]],dims)
		wx = reform(xz[0,*,*]*cos(wtau) - xz[1,*,*]*sin(wtau))
		wz = reform(xz[0,*,*]*sin(wtau) + xz[1,*,*]*cos(wtau))
		triangulate, wx, wz, tring
		rx = reform(xz[0,*,0])
		rz = reform(xz[1,0,*])
		sig = trigrid(wx,wz,raw,tring,xout=rx,yout=rz) > 0
	endif

	dct = cent[0]
	wtol = Scan_PD_tol(tol,pdstat=cur.pdstat,base=6)
	dum = How_many(fir=cnt,sec=lct,/nozero,whi=whi)
	case whi of
		0	:	ct = dct
		1	:	ct = long(cnt[0])
		2	:	begin
					zint = total(raw,2)
					dum = max(zint,ct)
					if abs(ct-dct) gt wtol then ct = dct
				end
		3	:	message, 'Either CENTER or LOCATION can be used, not both!'
	endcase

	sig = Img_int(raw[ct-hs:ct+hs,*],/xy_int,emod=apfl,_extra=_e)
	lbg = Img_int(raw[ct-hs-hoff:ct+hs-hoff,*],/xy_int,emod=apfl,_extra=_e)
	rbg = Img_int(raw[ct-hs+hoff:ct+hs+hoff,*],/xy_int,emod=apfl,_extra=_e)
	if keyword_set(coq) then begin
		sig[0,*] = Qvec(alp,sbet,lam=lam,/qtot)
		lbg[0,*] = (rbg[0,*] = Qvec(alp,sbet,dth=bdth,lam=lam,/qtot))
	endif

	res = Scan_clean(sig,lbg,rbg)
	case One_of(pxv,qyv,qzv) of
		-1	:	begin
					res[0,*] = sbet
					xtit = 'Beta'
				end
		0	:	begin
					res[0,*] = findgen((size(res))[2])
					xtit = 'Pixel'
				end				
		1	:	begin
					res[0,*] = (Qvec(alp,sbet,lam=lam))[1,*]
					dum = where(sbet ge 0)
					res = res[*,dum]
					xtit = 'Q!dy!n'
				end
		2	:	begin
					res[0,*] = (Qvec(alp,sbet,lam=lam))[2,*]
					xtit = 'Q!dz!n'
				end
	endcase
	Scan_show, res, tit = dtit, xtit = xtit, ytit = 'Counts', _extra = _e

	return
end