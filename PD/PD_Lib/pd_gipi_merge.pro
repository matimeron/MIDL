Function PD_gipi_merge, gid_dat= gdat, pin_dat= pdat, overlap= ovr, _extra=_e

;+
; NAME:
;		PD_GIPI_MERGE
; VERSION:
;		8.31
; PURPOSE:
;		Merging GID and Pinhole data.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = PD_GIPI_MERGE( GID_DAT = GDAT, PIN_DAT = PDAT [, keywords])
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	GID_DAT
;		2D GID data in the standard [4,M,N] format. 
;	PIN_DAT
;		2D Pinhole data in the standard [4,M,N] format.
;
;		Note 1:	Since from internal point of view there is no difference between
;				GID and Pinhole data, it is the user's responsibility to assign
;				GID_DAT and PIN_DAT correctly.
;		Note 2:	Some overlap between GID_DAT and PIN_DAT is required.
;	OVERLAP
;		Integer, then number of GID_DAT overlap points with PIN_DAT, on each
;		side.  If the number is too large, it'll be lowered.  Default value is 3
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to imbedded
;		routines.  Not to be used directly.
; OUTPUTS:
;		Returns the outcome of merging the GID and Pinhole data, in the
;		standard 2D format.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The GID and Pinhole data must have some overlap, both in the horizontal
;		and the vertical dimension.
; PROCEDURE:
; 		Uses the GID data to renormalize the Pinhole data, then splits the GID
; 		data into parts lying below and above (in the horizontal coordinate) the
; 		Pinhole data and grafts the pinhole data into the middle.
;		Calls PD_GID_INT.  Calls IMG_JOIN, IMG_REFORM, IMG_SCALE and SCAN_INTER,
;		from SPEC.  Calls DEFAULT, FPU_FIX, LINFIT_MM and POLEVAL, from MIDL.
; MODIFICATION HISTORY:
;		Created 10_JUN-2014 by Mati Meron.
;-

	on_error, 1
	ovr = Default(ovr,3,/dtyp)

	if (size(gdat,/dim))[0] eq 4 then begin
		gzi = Pd_GID_int(gdat,/z_int)
		gx = reform(gzi[0,*])
		gxn = n_elements(gx)
		gran = [min(gx,max=max),max]
	endif else message, 'Bad GID data!'

	if (size(pdat,/dim))[0] eq 4 then begin
		pzi = Pd_GID_int(pdat,/z_int)
		px = reform(pzi[0,*])
		dum = where(px ge gran[0] and px le gran[1],ndum)
		if ndum gt 0 then begin
			wpdat = pdat[*,dum,*]
			wpzi = pzi[*,dum]
			px = px[dum]
			pxn = n_elements(px)
			pran = [min(px,max=max),max]
		endif else message, 'No overlap, cannot continue!'
	endif else message, 'Bad Pinhole data!'

	sov = where(gx ge pran[0] and gx le pran[1])
	pzcomp = Scan_inter(pzi,gx[sov])
	rat = reform(pzcomp[1,*]/gzi[1,sov])
	coe = Linfit_mm(gx[sov],rat,ord=2)
	scam = (1/Poleval(px,coe))#replicate(1,(size(wpdat,/dim))[2])
	spdat = Img_scale(wpdat,scam)

	step = (pran[1]-pran[0])/(pxn-1)
	lo = where(gx le pran[0]-step,nlo,/null)
	if nlo gt 0 then hilo = lo[-1] else hilo = -1
	hi = where(gx ge pran[1]+step ,nhi,/null)
	if nhi gt 0 then lohi = hi[0] else lohi = gxn
	ovmax = ((lohi - hilo - 1)/2) > 1
	if ovr gt ovmax then $
	print, ovmax, form = '("	OVERLAP value too large, replaced by ",i0)'
	ovr = ovr < ovmax
	hilo = hilo + ovr
	lohi = lohi - ovr
	res = Img_join(gdat[*,0:hilo,*],spdat,gdat[*,lohi:-1,*],/tran,_extra=_e)
	rx = [gx[lo],px,gx[hi]]
	res = Img_reform(res,xvals=rx,/sort)

	return, FPU_fix(res)
end