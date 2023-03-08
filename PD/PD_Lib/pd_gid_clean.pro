Function PD_GID_clean, dat, wid, location = loc, bsmooth = bsm

;+
; NAME:
;		PD_GID_CLEAN
; VERSION:
;		8.331
; PURPOSE:
;		Performs a 2D background subtraction on GID data.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = PD_GID_CLEAN( DAT [,keywords])
; INPUTS:
;	DAT
;		2D data, either a single 2D array or the standard [4,M,N] representation
;		([3,M,N] is also acceptable)
; OPTIONAL INPUT PARAMETERS:
;	WID
;		Integer scalar (or 2-element vector) specifies how many columns on each 
;		side are to be averaged to get partial (one sided) background estimate.
;		If given as scalar, same width is used on both sides, else WID[0] is 
;		used on left and WID[1] on right.  Default value is 1.
; KEYWORD PARAMETERS:
;	LOCATION
;		2-element vector, specifies the location (in X-coordinates) of the 
;		columns to be used for background, on left and right.  By default, 
;		the extreme left and extreme right column(s) are used. 
;	BSMOOTH
;		Scalar, specifies the smoothing width for smoothing background data in
;		the Z direction.  Default is 1, i.e. no smoothing.  Binomial coefficient
;		smoothing is used.
; OUTPUTS:
; 		Returns 2D data in the same forma as the input, with the background 
; 		being subtracted.  The background is generated through linear 
; 		interpolation of 1D column data on left and right.
; OPTIONAL OUTPUT PARAMETERS:
; 		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		None other than the restrictions on data format listed above.
; PROCEDURE:
; 		Generates two sets of 1D data (in the Z direction) through the averaging
; 		of a number (1 or more) data columns on both sides of the region of 
; 		interest and propagates these sets, using linear interpolation, to 
; 		generate 2D background data.  Said background is subtracted from the 
; 		input data.  If DAT is of the full 2D [4,M,N] type, background errors
; 		are generated as well and combined in quadrature with the data errors.
;		Calls IMG_CLEAN, IMG_INT, PEAK_SMOOTH and SCAN_SCALE.  Calls DEFAULT 
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 1-OCT-2014 by Mati Meron, based on suggestions by Mike Henderson
;		from UC.
;		Modified 5-NOV-2014 by Mati Meron.  Bug fix and internal changes.
;-

	on_error, 1

	dim = size(dat,/dim)
	case n_elements(dim) of
		2	:	begin
					wdat = make_array(dim=[3,dim],typ=Type(dat)>4)
					wdat[0,*,*] = findgen(dim[0])#replicate(1.,dim[1])
					wdat[1,*,*] = replicate(1.,dim[0])#findgen(dim[1])
					wdat[2,*,*] = dat
				end
		3	:	if dim[0] eq 3 or dim[0] eq 4 then wdat = dat $
				else message, 'Bad data format!'
		else:	message, 'Bad or nonexistent data!'
	endcase
	wdim = size(wdat,/dim)
	efl = wdim[0] eq 4
	bdat = wdat

	wwid = Default(wid,1,/dtyp)
	wwid = ([wwid,wwid[0]])[0:1]

	x = reform(wdat[0,*,0])
	y = reform(wdat[1,0,*])
	case n_elements(loc) of
		0	:	wloc = [x[0],x[-1]]
		2	:	wloc = loc
		else:	message, 'Bad background locations!'
	endcase
	wloc = 0 > [min(wloc,max=max),max] < x[-1]
	dum = min(x - wloc[0],lloc,/abs)
	dum = min(x - wloc[1],hloc,/abs)
	iloc = [lloc,hloc]
	xloc = x[iloc]
	xspan = xloc[1] - xloc[0]

	ilo = (iloc[0] - wwid[0] + 1) > 0
	ihi = ilo + wwid[0] - 1
	if wwid[0] eq 1 then lbac = reform(wdat[1:2+efl,ilo,*],2+efl,dim[2]) $
	else lbac = Scan_scale(Img_int(wdat[*,ilo:ihi,*],/xy_int),1./wwid[0])
	lbac = Peak_smooth(lbac,wid=bsm)
	
	ihi = (iloc[1] + wwid[1] - 1) < (dim[1] - 1)
	ilo = ihi - wwid[1] + 1
	if wwid[1] eq 1 then hbac = reform(wdat[1:2+efl,ihi,*],2+efl,dim[2]) $
	else hbac = Scan_scale(Img_int(wdat[*,ilo:ihi,*],/xy_int),1./wwid[1])
	hbac = Peak_smooth(hbac,wid=bsm)

	for j = 0, dim[1]-1 do begin
		bdat[2,j,*]= ((xloc[1]-x[j])*lbac[1,*] + (x[j]-xloc[0])*hbac[1,*])/xspan
		if efl then bdat[3,j,*] = $
		sqrt((xloc[1]-x[j])^2*lbac[2,*]^2 + (x[j]-xloc[0])^2*hbac[2,*]^2)/xspan
	endfor

	tol = 1e-3*((x[1]-x[0]) < (y[1] - y[0]))
	res = Img_clean(wdat,bdat,/ref,tol=tol)
	if n_elements(dim) eq 2 then res = reform(res[2,*,*])
	
	return, res
end