Function PD_patch_gen, sp_0, sp_1, sp_2, sp_3, sp_4, sp_5, sp_6, sp_7, $
	orientation= ori, raw= raw, angles= ang, tilt= til, lo_cut=lct, hi_cut=hct,$
	force =frc, nscan= nsc, order= sord, slist= slis, factors= mfac, _extra= _e

;+
; NAME:
;		PD_PATCH_GEN
; VERSION:
;		7.15
; PURPOSE:
;		Patches together PD scans, for horizontal or vertical PD direction.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		PD_PATCH_GEN, SP_0 [, SP_1, ....] [,keywords]
; INPUTS:
;	SP_0, SP_1, ....
;		List(s) of Pilatus detector scan numbers, provided in any form that is
;		acceptable (individually) by ARRPACK.  The lists will be combined into
;		a single list, internally
;
;		Note:	The ordering of SP_0, ... SP_N is arbitrary.
;
;		Alternatively, SP_0, SP_1,... can be actual integrated (1D) scans.  In 
;		such case all the keywords above, with the exception of LO_CUT, HI_CUT,
;		FORCE, NSCAN and FACTORS are disabled.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ORIENTATION
;		Character string input specifying PD orientation.  Two possible inputs,
;		"horizontal" and "vertical" (first letter suffices). Alternatively can
;		be provided as numerical input, with 0 signifying horizontal and 1,
;		vertical.
;	/RAW
;		Switch.  If set, the data is read "raw", meaning the X and Y
;		coordinates are expressed in pixels, not angles or Q-values.
;	/ANGLES
;		Switch.  If set the coordinates of the image are the angles DTHETA (for
;		horizontal) and BETA for vertical, instead of Q_xy and Q_z.
;	TILT
;		Optional detector tilt angle (couterclockwise) in degrees.  Valid for
;		angle and Q-xy coordinates, has no effect if /RAW is set.
;	LO_CUT
;		Specifies the low cut of the data i.e. the level below which the data
;		should be cut.  For horizontal direction the low cut applies in the
;		horizontal dimension, and for vertical, in the vertical.  Given as
;		numbers in the [0,1] range, so that low cut of, say, 0.1, means "cut
;		data from 10% of the range and down".  The cut applies to raw data,
;		before processing.
;
;		Note:	LO_CUT is never applied to the first scan on the list.
;	HI_CUT
;		Same as LOW_CUT, for the high end of the data.  Thus high cut of, say,
;		0.8, means "cut data from 80% of the range and up".  The cut applies to
;		raw data, before processing.
;
;		Note:	HI_CUT is never applied to the last scan on the list.
;	/FORCE
;		Switch.  If set, forces junction even when there is no overlap between
;		some scans.  This involves extrapolation which greatly increases
;		uncertainties, so it should be used with care.
;	NSCAN
;		Optional output, see below.
;	ORDER
;		Optional output, see below.
;	SLIST
;		Optional output, see below.
;	FACTORS
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  This includes (among others) the following keywords,
;		affecting data readout:
;
;			HROI
;				Two element vector defining horizontal region of interest,
;				in *pixels*.
;			VROI
;				Two element vector defining vertical region of interest, in
;				*pixels*.
;			/BAD
;				Switch.  If set, faulty (very high count rate) pixels are
;				removed from the data.
;			/NORM
;				Switch.  If set, the data is normalized to monitor counts.
;
;		See SCAN_PD_READ for more details.
;
;		Note:	/NORM is set by default in PD_PATCH_GEN, it can be disabled by
;				using NORM = 0 in the function call.
; OUTPUTS:
;		Returns the patched data in the standard [3,*] form.
; OPTIONAL OUTPUT PARAMETERS:
;	NSCAN
;		Returns the number of scans present.
;	ORDER
;		Returns the order of the scans, in SNUM.
;	SLIST
;		Returns the list of scans in SNUM, ordered by ORDER.
;	FACTOR
;		A vector of length N (the number of scans present) returning the
;		multiplicative factors of the matches.  The i-th value is the factor by
;		which scan #i is multiplied relative to scan #0.  The 0-th value is
;		always 1.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The scans on the list must exist and, unless /FORCE is set, each scan
;		should have at least one-point overlap with some other scan.
; PROCEDURE:
;		Reads the scans, orders then (by the value of DTH for horizontal
;		orientation or of BETA for vertical), and joins them together.  Calls
;		IMG_INT, SCAN_ANG_ORDER, SCAN_JOIN, SCAN_PD_CENTER and SCAN_PD_READ,
;		from SPEC.  Calls ARREQ, ARRPACK, DEFAULT and FPU_FIX, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-NOV-2007 by Mati Meron.
;		Modified 15-NOV-2007 by Mati Meron.  Added keyword HI_CUT.
;		Modified 25-APR-2008 by Mati Meron.  Internal changes.
;		Modified 20-JUN-2008 by Mati Meron.  Internal changes.
;		Modified 15-AUG-2008 by Mati Meron.  Internal changes.
;		Modified 25-AUG-2008 by Mati Meron.  Internal changes.
;		Modified 15-APR-2009 by Mati Meron.  Added keyword LO_CUT.
;		Modified 25-JUN-2009 by Mati Meron.  Added option of integrated scan
;		inputs.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1
	checknam = ['Beta','Dth']
	del = 1e-3

	np = Npardef(sp_0, sp_1, sp_2, sp_3, sp_4, sp_5, sp_6, sp_7,which=whi)
	if np eq 0 then message, 'Input data missing!'
	nmx = 8
	snam =  strcompress('sp_' + sindgen(nmx),/remove)
	dum = execute('siz = size(' + snam[whi[0]] + ')')
	if Arreq(siz[0:1],[2,3]) then begin
		nsc = np
		wlct = 0. > Default(lct,0.,/dtyp) < 1.
		if (size(wlct))[0] eq 0 then wlct = replicate(wlct,nsc) $
		else wlct = ([wlct,replicate(0.,nsc)])[0:nsc-1]
		wlct[0] = 0.
		whct = wlct > Default(hct,1.,/dtyp) < 1.
		if (size(whct))[0] eq 0 then whct = replicate(whct,nsc) $
		else whct = ([whct,replicate(1.,nsc)])[0:nsc-1]
		whct[nsc-1] = 1.
		mfac = replicate(1.,nsc)
		for i = 0, np-1 do begin
			dum = execute('next =' + snam[whi[i]])
			q = next[0,*]
			minq = min(q,max=maxq)
			check = (q - minq)/(maxq-minq)
			dum = where(check ge wlct[i] and check le whct[i])
			next = next[*,dum]
			if i gt 0 then begin
				res = Scan_join(res,next,force=frc,fact=fac,_extra=_e)
				mfac[i] = fac[1]
			endif else res = next
		endfor
	endif else begin
		stem = Arrpack(sp_0, sp_1, sp_2, sp_3, sp_4, sp_5, sp_6, sp_7)
		cent = Scan_PD_center(stem,ori=ori,dim=dim)
		sord = Scan_ang_order(sp_0,sp_1,sp_2,sp_3,sp_4,sp_5,sp_6,sp_7,$
			ord=checknam[1-ori],list=slis,_extra=_e)
		nsc = n_elements(sord)
		slis = slis[sord]
		check = reform((fildat.scan[slis].angs)[ori+1,*])
		ccheck = reform((fildat.scan[slis].varan)[ori+1,*])
		cfl = (max(check,min=min) - min) lt del and Arreq(-1,where(ccheck))
		if not cfl then message, 'Non-constant ' + checknam[ori] + '!'
	
		hro = (vro = fltarr(2,nsc))
		hro[1,*] = replicate(dim[0]-1,nsc)
		vro[1,*] = replicate(dim[1]-1,nsc)
	
		wlct = 0. > Default(lct,0.,/dtyp) < 1.
		if (size(wlct))[0] eq 0 then wlct = replicate(wlct,nsc) $
		else wlct = ([wlct,replicate(0.,nsc)])[0:nsc-1]
		wlct[0] = 0.
		llim = round((dim[ori]-1)*wlct)
		whct = wlct > Default(hct,1.,/dtyp) < 1.
		if (size(whct))[0] eq 0 then whct = replicate(whct,nsc) $
		else whct = ([whct,replicate(1.,nsc)])[0:nsc-1]
		whct[nsc-1] = 1.
		hlim = round((dim[ori]-1)*whct)
	
		if ori then begin
			vro[0,*] = llim
			vro[1,*] = hlim
		endif else begin
			hro[0,*] = llim
			hro[1,*] = hlim
		endelse
	
		mfac = replicate(1.,nsc)
		for i = 0l, nsc-1 do begin
			idat = Scan_PD_read(slis[i],orient=ori,hroi=hro[*,i],vroi=vro[*,i],$
				/norm, raw= raw, angles= ang,tilt= til,_extra = _e)
			next = Img_int(idat,xy_int=ori,z_int=1-ori,_extra=_e)
			if i gt 0 then begin
				res = Scan_join(res,next,force=frc,fact=fac,_extra=_e)
				mfac[i] = fac[1]
			endif else res = next
		endfor
	endelse

	return, FPU_fix(res)
end