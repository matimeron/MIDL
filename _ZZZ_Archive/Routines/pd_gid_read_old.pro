Function PD_GID_read_old, snum, frame = fnum, progress = prg, verify = ver, $
	hroi = hroi, vroi = vroi, slit = sli, yoff = yof, raw = raw, angles = ang, $
	signed = sgn, relaxed = rel, sin_norm = snr, pol_corr = plc, bypass = byp, $
	xy_int= xin, z_int= zin, vfac= vfc, patch_range= ptr, title= tit, _extra= _e

;+
; NAME:
;		PD_GID_READ
; VERSION:
;		8.422
; PURPOSE:
;		Reads a set of PD scans, integrates each horizontally and joins them
;		into a 2D image.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = PD_GID_READ( SNUM [,keywords])
; INPUTS:
;	SNUM
;		A list of scan numbers, in any form recognizable by RANGE_PROC.  If more
;		than one scan is provided, PD_GID_READ will attempt patching, first in 
;		the horizontal direction then, if needed, in the vertical direction.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	FRAME
;		Accepts a frame number or list of frame numbers, in any form
;		recognizable by RANGE_PROC.  Valid only when a single scan is used,
;		automatically disabled for multiple scans.
;		Note that the default operation is always to use all frames.
;	/PROGRESS
;		Switch.  If set, prints informational progress messages to the screen.
;	/VERIFY
;		Switch.  If set, the existence of all frames to be processed is
;		verified and bad or missing frames are rejected.
;	HROI
;		Two element vector defining horizontal region of interest, in *pixels*.
;	VROI
;		Two element vector defining vertical region of interest, in *pixels*.
;	SLIT
;		Numeric scalar, the value of the "eletronic slit" (around the center
;		pixel) in *pixels*.  Defines HROI which overrides directly given HROI
;		(if any).
;	YOFF
;		Numeric scalar, specifying Y_offset along the beam footprint.  If given,
;		results in appropriate offset in HROI.  Ignored if HROI or SLIT not 
;		given.
;	/RAW
;		Switch.  If set, the coordinates of the image are "frame #" for
;		horizontal and "pixel #" for vertical, instead of Q_xy and Q_z.  Note
;		that if multiple scans are patched, the frame number listed does not
;		correspond to a frame number in any specific scan.
;	/ANGLES
;		Switch.  If set the coordinates of the image are the angles DTHETA for
;		horizontal and BETA for vertical, instead of Q_xy and Q_z.
;	/SIGNED
;		Switch.  If set, Q_xy is multiplied by the sign of the DTH angle.
;		/SIGNED has no effect when /RAW or /ANGLES is set.
;	/RELAXED
;		Switch.  If set, the internal transformation from angles to Q-values is
;		unconstrained.  By default it is constrained to the internal data
;		region, to avoid edge inaccuracies.
;	/SIN_NORM
;		Specifies additional geometric normalization, consisting of multiplying
;		each column by Sin(dth)/Sin(min(dth)).  This corrects for the changing
;		field of view in GID scans.
;
;		Note:  	/SIN_NORM is set by default in PD_GID_READ, it can be disabled
;				by using SIN_NORM = 0 in the function call.
;	/POL_CORR
;		Switch.  When set, the data is corrected for polarization.  Assumes 
;		initial horizontal polarization.
;
;		Note:  	/POL_CORR is set by default in PD_GID_READ, it can be disabled
;				by using POL_CORR = 0 in the function call.
;	/BYPASS
;		Switch.  If set, it is assumed that all the scans correspond to the 
;		same BETA angle.  For internal use only.
;	/XY_INT											|	At most one
;		Switch.  Specifies integration over Qxy.	|	of these two
;	/Z_INT											|	keywords may
;		Switch.  Specifies integration over Qz. 	|	be set.
;	/VFAC
;		Switch.  If set, the vertical patching factor(s) is printed to the 
;		screen.  For diagnostic purposes.
;	PATCH_RANGE
;		The range of channels to use in patching (only when more than one data
;		set is present).  Note that PATCH_RANGE values are taken *after* VROI
;		is applied.
;	TITLE
;		Optional output (and input), see below.
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
;
;		Note:	/NORM is set by default in PD_GID_READ, it can be disabled by
;				using NORM = 0 in the function call.
; OUTPUTS:
;		Returns a 3D array of dimensions [4,NP,NL] (where NP is the
;		number of PD frames used and NL is the long dimension of the PD) such
;		that
;			Result[0,*,*] = Q_xy coordinates of the image.
;			Result[1,*,*] = Q_z coordinates of the image.
;			Result[2,*,*] = The image itself.
;			Result[3,*,*] = The statistical errors of the image data.
;
;		However, see /ANGLES above for alternative coordinates.
;
;		Note that NP may be lower if data patching takes place and NL may be
;		lower, if VROI is used, or higher if vertical patching takes place.
; OPTIONAL OUTPUT PARAMETERS:
;	TITLE
;		If provided, returns the same, else returns a character scalar, made of
;		the file name and the scan numbers, to be used as title for plots.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		1)  The scan list provided must include only valid PD scans.
;		2)	The value of LAMBDA must be constant for all the scans and equal
;			the calibration value (to within 1e-3 relative).
;		3)  The values of ALPHA must be constant for all the scans
;			(to within 1e-3).
;		4)	If BYPASS is set, the values of BETA must also be constant for all
;			scans, to same accuracy as ALPHA.
; PROCEDURE:
;		Reads the data, evaluates Q_z and Q_xy using calibration values, then
;		translates the Q values to rectalinear coordinates and interpolates
;		the data using triangulation.
;		Calls IMG_INT, IMG_JOIN, IMG_REFORM, IMG_TRIG, SCAN_FIELD READ, 
;		SCAN_ORDER, SCAN_PD_COMB, SCAN_PD_COMB_JOIN, SCAN_PD_FRAMES, 
;		SCAN_PD_LCOO and SPEC_FILE_CHECK from SPEC.  Calls itself recursively as
;		needed.  Also calls DEFAULT, DIF, FPU_FIX, PRUNE, RANGE_COMP, 
;		RANGE_PROC, SIGN, SORPURGE and STRPARSE_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created 1-NOV-2007 by Mati Meron, based on LD_READ.
;		Modified 10-APR-2008 by Mati Meron.  Internal changes.
;		Modified 10-MAY-2008 by Mati Meron.  Changed input scheme and added
;		keyword RAW.
;		Modified 20-JUN-2008 by Mati Meron.  Internal changes.
;		Modified 25-JUL-2008 by Mati Meron.  Internal changes.
;		Modified 15-Aug-2008 by Mati Meron.  Internal changes.
;		Modified 5-MAR-2009 by Mati Meron.  Added keyword SIN_NORM.
;		Modified 15-AUG-2009 by Mati Meron.  Increased maximal number of scans
;		from 8 to 16.  Some internal changes.
;		Modified 1-NOV-2009 by Mati Meron.  Added keyword FRAME, enabling 
;		selective frame reading capability, for single scans.
;		Modified 5-FEB-2010 by Mati Meron.  Added keyword VERIFY.
;		Modified 20-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 5-APR-2011 by Mati Meron.  Added keyword YOFF.
;		Modified 5-OCT-2011 by Mati Meron.  Internal changes.
;		Modified 15-AUG-2012 by Mati Meron.  Internal changes.
;		Completely rewritten 10-APR-2013 by Mati Meron.  Improved patching.
;		Eliminated keyword VERBOSE (which was never used).
;		Modified 25-OCT-2013 by Mati Meron.  Enabled variable spacing of result
;		Qxy coordinates.
;		Modified 5-NOV-2013 by Mati Meron.  Added keyword PROGRESS.  Corrected
;		interpolation instability.
;		Modified 10-NOV-2013 by Mati Meron.  Added keyword SLIT (moved from 
;		PD_GID).
;		Modified 30-NOV-2013 by Mati Meron.  Added keywords XY_INT and Z_INT.
;		Modified 15-FEB-2014 by Mati Meron.  Added keywords POL_CORR and BYPASS.
;		Modified 30-MAR-2014 by Mati Meron.  Internal changes.
;		Modified 15-JUN-2014 by Mati Meron.  Internal changes.
;		Modified 15-JUL-2014 by Mati Meron.  Internal changes.
;		Modified 10-AUG-2015 by Mati Meron.  Added keyword VFAC.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	if keyword_set(byp) then begin
		Spec_file_check, snum,/pil,par_con=['In_Rot','Out_Rot'],tol=1e-3,$
		nsc=nsc,lis=lis,_extra=_e
		if nsc gt 1 then wfnum = (cfl = -1) $
		else wfnum = Scan_PD_frames(lis[0],fnum,verify=ver,/uni,comp=cfl)		
		sord = Scan_order(lis,col=[0,-1,-2])
		slis = lis[sord]
	
		if keyword_set(prg) then begin
			prfl = 1
			pform = '("	Scan #",i0'
			yofl = Isnum(yof)
			print
		endif else prfl = 0
	
		if n_elements(sli) eq 1 then $
		hroi= (Scan_PD_center(slis[0]))[0]+round(sli)/2*[-1,1]
		snr = Default(snr,1,/dtyp)
		dx = fltarr(nsc)
		for i = 0l, nsc-1 do begin
			next = Scan_PD_comb(slis[i],wfnum,ori=1,hroi=hroi,vroi=vroi,$
			yoff=yof,ver=ver,sin=snr,/reg,/norm,tit=dtit,hrof=off,_extra=_e)
			if prfl then begin
				if yofl then print,slis[i],hroi+off[0],hroi+off[1],form=pform+ $
				'," HROI range: [",i0,", ",i0,"] - [",i0,", ",i0,"]")' $
				else print, slis[i], form = pform + ')'
			endif
			xtem = reform(next[0,*,0])
			dx[i] = (max(xtem,min=min)-min)/(n_elements(xtem)-1)
			if i eq 0 then res = next $
			else res = Scan_PD_comb_join(res,next,/auto,pat=ptr,_extra=_e)
		endfor
		good = Prune(reform(res[0,*,0]),min(dx)/2,/gap,/strict)
		res = res[*,good,*]
	
		dtit = strmid(dtit,0,strpos(dtit,'_')) + ' S# ' + Range_comp(slis)
		if nsc eq 1 and not cfl then dtit = dtit + ' (' + Range_comp(wfnum)+ ')'
		tit = Default(tit,dtit)

		dim = (size(res))[2:3]
		if keyword_set(raw) then begin
			res[0,*,*] = findgen(dim[0])#replicate(1.,dim[1])
			res[1,*,*] = res[1,*,*] + ((Default(vroi,0))[0] > 0)
		endif else begin
			dth = reform(res[0,*,*])
			dthref = reform(dth[*,0])
			bet = Scan_PD_lcoo(slis[0],dir='ver',vroi=vroi,/ang,ver=ver)
			bet = replicate(1.,dim[0])#bet
			if Default(plc,1,/dtyp) then $
			pfac = 1. - (sin(!dtor*dth)*cos(!dtor*bet))^2 else pfac = 1.
			dat = reform(res[2,*,*]/pfac)
			err = reform(res[3,*,*]/pfac)
			if keyword_set(ang) then begin
				qxy = dth
				qz = bet
				qxyref = dthref
			endif else begin
				lam= (Scan_field_read(slis,'Lambda',/const,tol=1e-3,cfl=cfl))[0]
				if not cfl then message, 'Missing or non-constant Lambda'
				alp = $
				(Scan_field_read(slis,'angs',/const,tol=1e-3,rel=0,cfl=cfl))[0]
				if cfl[0] then alp = !dtor*alp $
				else message, 'Missing or non-constant Alpha'
				dth = !dtor*dth
				bet = !dtor*bet
				k = 2*!pi/lam
				apb = (alp + bet)/2
				amb = (alp - bet)/2
				qxy = $
				2*k*sqrt((sin(apb)*sin(amb))^2 + cos(alp)*cos(bet)*sin(dth/2)^2)
				if keyword_set(sgn) then qxy = qxy*Sign(dth)
				qz = 2*k*sin(apb)*cos(amb)
				qxyref = 2*k*cos(alp)*sin(!dtor*dthref/2)
			endelse
			if keyword_set(rel) then tight = 0 else tight = 1
			tdat = Img_trig(dat,qxy,qz,/xsm,xtr=tqxy,ytr=tqz,min=tight)
			terr = Img_trig(err,/last)
			tdim = [n_elements(tqxy),n_elements(tqz)]
			res = fltarr([4,tdim])
			res[0,*,*] = tqxy#replicate(1.,tdim[1])
			res[1,*,*] = replicate(1.,tdim[0])#tqz
			res[2,*,*] = tdat
			res[3,*,*] = terr
			tol = min(Dif(qxyref,/lin))/10
			res = Img_reform(res,tol,xvals=qxyref)
		endelse
	endif else begin
		wsnum = Range_proc(snum,/uni)
		bet = (Scan_field_read(wsnum,'Angs'))[*,1]
		rbet = 1e-2*round(1e2*bet)
		slis = Sorpurge(rbet,net=n)
		for i = 0l, n-1 do begin
			ilis = where(rbet eq rbet[slis[i]])
			tres = Pd_gid_read(wsnum[ilis],fram= fnum,prog= prg,ver= ver, $
				hroi= hroi,vroi= vroi,slit= sli,yoff= yof,raw= raw,angles= ang,$
				sign= sgn,relax= rel,sin_norm= snr,pol_corr= plc,/bypass, $
				patch_range= ptr,title= tit,_extra= _e)
			if i eq 0 then begin
				res = tres
				qxyref = reform(res[0,*,0])
				tol = min(Dif(qxyref,/lin))/10
			endif else begin
				res = Img_join(res,tres,/ver,fac=fac,_extra=_e)
				if keyword_set(vfc) then print, string(10b) + $
				'	Vertical factor = ', fac[1]
			endelse
		endfor

		if n gt 1 then begin
			res = Img_reform(res,tol,xvals=qxyref)
			nlis = Strparse_mm(tit,' ',tlis)
			if nlis ge 2 then begin
				tlis[2] = Range_comp(wsnum)
				tit = strjoin(tlis,' ')
			endif
		endif
	endelse

	case One_of(xin,zin) of
		-1	:
		0	:	res = Img_int(res,/xy_int)
		1	:	res = Img_int(res,/z_int)
	endcase

	return, FPU_fix(res)
end