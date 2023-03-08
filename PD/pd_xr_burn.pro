Pro PD_XR_burn, snum, fnum, slit = sli, back_ang = bag, left= lef, right= rig, $
	result= res, _extra=_e

;+
; NAME:
;		PD_XR_BURN
; VERSION:
;		8.42
; PURPOSE:
;		Analyzes XRF "burn test" data.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		PD_XR_BURN, SNUM [, FNUM] [, keywords])
; INPUTS:
;	SNUM
;		Single scan number.
; OPTIONAL INPUT PARAMETERS:
;	FNUM
;		Frame number(s) within the scan.  If not given, defaults to all frames.
; KEYWORD PARAMETERS:
;	SLIT
;		A 2 element vector specifying electronic slit dimensions, in pixels, in
;		[horizontal,vertical] order.
;	BACK_ANG
;		Numeric scalar, the "sideways" offset angle used to evaluate background,
;		in degrees.  If not given, no background subtraction is performed.
;	/LEFT
;		Switch.  Specifies taking left sided background.
;	/RIGHT
;		Switch.  Specifies taking right sided background.
;
;		Note:	By default, double sided background is used.  Therefore, setting
;				both LEFT and RIGHT is same as not setting both.
;	RESULT
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass keywords to imbedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		Screen output only.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the data being displayed (reflectivity as function os time) in
;		the standard 3 column format.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		SNUM must represent proper burn data, i.e. data taken without changing
;		any of the angles.  Moreover:
;			Alpha and Beta must be equal.
;			Phi (Dth) must be zero.
; PROCEDURE:
; 		Straightforward, extracts the data as if it was regular reflectivity
; 		data then displays it as function of time into measurement.  Calls
; 		SCAN_COLUMN, SCAN_PD_FRAMES, SCAN_PD_XR, SCAN_Q_ANGS, SCAN_SHOW
; 		and SPEC_FILE_CHECK, from SPEC.  Calls LEGEND_MM, LINFIT_MM and POLEVAL,
; 		from MIDL.
; MODIFICATION HISTORY:
;		Created 20-JUN-2015 by Mati Meron.
;-

	on_error, 1
	del = 1e-3

	Spec_file_check, snum, /sing, /pildet, list = wsnum
	wfnum = Scan_PD_frames(wsnum,fnum,nframes=nfr)
	ang = Scan_q_angs(wsnum,wfnum)
	min = min(ang,dim=2,max=max)
	if max(max-min) le del then begin
		mang = total(ang,2)/nfr
		if abs(mang[1]-mang[0]) le del and abs(mang[2]) le del then begin
			res= Scan_PD_XR(wsnum,wfnum,sli=sli,bac=bag,lef=lef,rig=rig,tit=tit)
			tim = (Scan_column(wsnum,'seconds'))
			res[0,*] = (total(tim,/cum))[wfnum]
		endif else message, 'Not reflectivity data!'
	endif else message, 'Changing angle(s), not a burn test!'

	coe = Linfit_mm(reform(res[0,*]),alog(reform(res[1,*])),$
	reform(res[1,*]/res[2,*]),err=err)
	fres = res
	fres[1,*] = exp(Poleval(fres[0,*],coe))
	fres[2,*] = 0*fres[2,*]
	tau = -1/coe[1]
	del = tau^2*err[1]
	tdig = 2 > ceil(alog10(abs(tau))) < 6
	ddig = 2 > ceil(alog10(abs(del))) < 6
	form = '("!7s!x = ",' + string(tdig+7,tdig,form='("g",i0,".",i0,",")')+$
		'" (",'+ string(ddig+7,ddig,form='("g",i0,".",i0,",")')+ '") sec.")'
	txt = strcompress(string(tau,del,form=form))
	if tau ge 0 then loc = 'UR' else loc = 'UL'

	Scan_show, res, fres, thi = 2, /ynoz, xtit = 'Time (sec)',ytit= 'Counts', $
	tit= tit, xmar= [12,3], ymar= [4,4], lcol=[!pcol.blue,!pcol.cyan], _extra=_e
	Legend_mm, text = txt, loc = loc, /nowrap

	return
end