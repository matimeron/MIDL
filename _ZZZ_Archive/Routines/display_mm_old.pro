Pro Display_mm_old, img, x, y, ind= ind, image= imag, contour= cont, surface= surf,$
	order = ord, xoff = xof, yoff = yof, xrev = xrv, yrev = yrv, rot = rot, $
	bin = bin, topsiz = tps, aver = ave, zoom = zom, auzoom = auz, $
	pix = pix, fine = fin, min = imn, max = imx, amax = amx, block = blk, $
	log = log, intype = itp, apply = fun, param = par, grid = gri, gstyle= gst,$
	mark = mrk, radius = rad, window= win, wnew= new, noerase= ner, even= eve, $
	shave= shv, expad = epd, poff = pof, clean= cln, nodata= nod, restore= rst,$
	isize = isi, wsize = wsi, ppos = pps, range = ran, _extra = _e

;+
; NAME:
;		DISPLAY_MM
; VERSION:
;		8.215
; PURPOSE:
;		Image display.
; CATEGORY:
;		Display.
; CALLING SEQUENCE:
;		DISPLAY_MM, IMG [, X, Y] [keywords]
; INPUTS:
;	IMG
;		A two dimensional numeric array.  Mandatory.
;		Optionally, a 3D array may be used.  In such case, IMG[2,*,*] is taken
;		to be the actual image, while IMG[0,*,*] and IMG[1,*,*] are taken to be
;		X and Y (see below), respectively.  However, see /IND below.
; OPTIONAL INPUT PARAMETERS:
;	X
;		An optional vector or 2D array of X values for the image.  If not
;		provided, an internal vector (containing consecutive pixel numbers)
;		is generated.
;
;		Optionally, X may be given as a [2,*,*] array.  In such case, X[0,*,*]
;		is used as X and X[1,*,*] as Y, internally.
;	Y
;		An optional vector or 2D array of Y values for the image.  If not
;		provided, an internal vector (containing consecutive pixel numbers) is
;		generated.
; KEYWORD PARAMETERS:
;	IND
;		Integer scalar.  If IMG is a 3D array, IND specifies the index of the
;		image within the array.  In other words, IMG[IND,*,*] is the actual
;		image.  The default value for IND is 2.
;	/IMAGE
;		Switch.  Specifies an Image (TV) display using IDL TVSCL.  This is also
;		the default in the absence of any specification.
;	/CONTOUR
;		Switch.  Specifies contour display using IDL CONTOUR.
;	/SURFACE
;		Switch.  Specifies surface display using IDL SHADE_SURF.
;	ORDER
;		Scalar integer, overrides the setting of !ORDER.  All even values
;		translate to 0, all odd to 1.  The external value of !ORDER is
;		unaffected.  Note that !order only inluences IMAGE, not the other modes.
;	XOFF
;		Scalar, optional X-axis offset.
;	YOFF
;		Scalar, optional y_axis offset.
;	/XREV
;		Switch.  If set, the X vector is reversed.
;	/YREV
;		Switch.  If set, the Y vector is reversed.
;	ROT
;		An integer scalar specifying image rotation.  See IDL function ROTATE
;		for details.
;	BIN
;		An integer scalar or a vector of length 2, specifies binning of the
;		image.  If given as a vector, first entry applies to the X dimension and
;		second to Y.  If given as a scalar, it applies to both dimensions.
;
;		Note:	When binning, the input array(s) may be trimmed (on the high
;				end) to make their dimensions evenly divisible by BIN.
;	TOPSIZ
;		An integer scalar or a vector of length 2, specifies maximal allowed
;		size (in pixels) for the image.  If given as 1, defaults to 512x512.
;		When BIN is provided. TOPSIZE has no effect. 
;	/AVER
;		Switch.  Specifies that the binned channels are to be averaged, instead
;		of scaled by the bin ratio (which is the default).  If BIN is not used,
;		/AVER has no effect.
;	ZOOM
;		Same as bin, but specifies zooming, i.e. scaling up of the image.  ZOOM
;		is only active in the IMAGE mode.
;	AUZOOM
;		Specifies automatic zooming to a size nearer to AUZOOMxAUZOOM if the
;		value of AUZOOM is >1, or to 512x512 otherwise.  Active only in IMAGE
;		mode.  Overriden by ZOOM if the later is provided.
;		Note:  AUZ can be given as 2-element vector, in which case the
;		components are applied individually to the X and Y dimensions.
;	/PIX
;		Switch.  Specifies keeping the pixels as is (no interpolation) while
;		zooming.  Equivalent to the REBIN keyword /SAMPLE.  If no zooming is
;		performed, /PIX has no effect.
;	/FINE
;		Switch.  Results in finer mesh when diplaying in CONTOUR mode with /LOG
;		set.  No effect in other modes.
;	MIN
;		A scalar entry in the range (0,1) specifying minimal relative value to
;		be displayed.  The minimum is set at MIN*maximum(IMG) and all image
;		values less than the minimum are set to the minimum.
;
;		Note:	If minimum(IMG) < 0 then display minimum is set at
;		minimum(IMG) + MIN*(maximum(IMG) - minimum(IMG))
;	MAX
;		Same as minimum, for the maximal value to be displayed.  All image
;		values greater than the maximum are set to the maximum.
;	AMAX
;		Specifies an absolute (not relative) maximum for the image.  Providing
;		a nonzero value for AMAX results in said value being placed in the upper
;		left corner of the image and, in case AMAX is lower then the image
;		maximum, in effectively setting MAX (see above) to AMAX/Max(image).
;		AMAX overrides MAX but doesn't interfere with MIN.
;	/BLOCK
;		Switch.  If set and MAX is provided, all the values greater than the
;		maximum are set to the minimum (i.e. effectively zero).
;	/LOG
;		Switch.  If set, LOG(IMG) is displayed.  However, see /INTYPE.
;	/INTYPE
;		Switch.  If set and /LOG is set, log(IMG + 1) is displayed.  This is
;		also true without setting /INTYPE if IMG is of one of the integer types.
;	APPLY
;		String scalar, name of a function which, if provided, is applied to IMG,
;		so that function(IMG) is displayed insted of IMG itself.
;	PARAM
;		An arbitrary value or variable which is passed to the function FUN.
;	/GRID
;		Switch.  If set, a grid is displayed on top of the image (only in IMAGE
;		mode).
;	GSTYLE
;		Integer scalar, specifies line style for GRID (see plot linestyles).
;		Default value is 1, i.e. dotted line.
;	MARK
;		Numeric array, providing region(s) of interest coordinates.  Can be
;		either 2D, in a [4,*] format, or 1D with a number of entries divisible
;		by 4.  Each set of 4 entries defines region of interest in a
;		[Low_x,Low_y,High_x,High_Y] order.  The region of interest will be
;		marked on the image in white.  MARK is only active in IMAGE mode.
;	RADIUS
;		Value of radius to round the corners of the rectangles made by MARK.
;		If MARK is not present, RADIUS has no effect.
;	WINDOW
;		The number of the graphics window to use.  Optional, if not given TV_MM
;		will pick a window by itself.
;	/WNEW
;		Switch.  Forces the creation of new graphics window.
;
;		Note:	If /WNEW is not set, TV_MM will attempt to use the window
;				with number given by WINDOW or, if none was given, the window
;				corresponding to !D.WINDOW.  In the IMAGE mode, however, if this
;				is not large enough to accomodate the image, a new window will
;				still be created.
;	/NOERASE
;		Switch.  If set, the window is not erased prior to image display.
;		Default is "erase".  Active only in the IMAGE mode.
;	/EVEN
;		Switch.  If set, the window sizes are rounded upward to even numbers.
;	/SHAVE
;		Switch.  If set, the margins around the image are reduced ("shaved").
;	EXPAD
;		Extra "padding" to be applied to the size of the graphics window (active
;		only in IMAGE mode).  Can be given as a 2-element vector, in a [x_pad,
;		y_pad] format, or as a scalar (in which case same padding applies to 
;		both dimensions).
;	POFF
;		An offset of the image into the graphics window (active only in IMAGE
;		mode).  Can be given as a 2-element vector, in a [x_off,y_off] format,
;		or as a scalar (in which case same offset applies to both dimensions).
;
;		Note:	The offset is applied after the graphics window has already been
;				established and it may be reduced (even to zero) if the window
;				is not large enough for the full offset.
;	/CLEAN
;		Switch. If set, only the image is displayed, without axes and scales.
;		Active only in the IMAGE mode.
;	/NODATA
;		Switch.  Same as the /NODATA keyword to PLOT, causes no data to be
;		displayed.  Active only in the IMAGE mode.
;	/RESTORE
;		Switch.  If set, !D.WINDOW is reset to its original value (prior to the
;		call to DISPLAY_MM, on exit.
;	ISIZE
;		Optional output, see below.
;	WSIZE
;		Optional output, see below.
;	PPOS
;		Optional output, see below.
;	RANGE
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to pass all keywords acceptable by PLOT, CONTOUR
;		or SHADE_SURF, as the case arises.  Not to be used directly.
; OUTPUTS:
;		None.
; OPTIONAL OUTPUT PARAMETERS:
;	ISIZE
;		Returns the size (in pixels) of the image, as a 2D long integer vector.
;		Active only in IMAGE mode.
;	WSIZE
;		Returns the size (in pixels) of the minimal window needed to display the
;		image, as a 2D long integer vector.  Active only in IMAGE mode.
;	PPOS
;		Returns the "plot position" (see !P.POSITION) in device units.  Active
;		only in IMAGE mode.
;	RANGE
;		Returns the X and Y coordinate ranges of the image in same format as
;		PPOS, i.e. [X_min, Y_min, X_max, Y_max].  Active only in IMAGE mode.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Calls ARREQ, DEFAULT, FLTROUND, FPU_FIX, ISNUM, 
;		ONE_OF, PLVAR_KEEP, TOLER and WHERINSTRUCT, from MIDL.
; MODIFICATION HISTORY:
;		Created 30-NOV-2005 by Mati Meron as upgrade from the earlier TV_MM.
;		Modified 15-DEC-2005 by Mati Meron.  Changed keyword NEW to WNEW.
;		Modified 15-FEB-2006 by Mati Meron.  Added keyword POFF and some
;		internal changes.
;		Modified 1-MAR-2006 by Mati Meron.  Added keyword RESTORE.
;		Modified 15-JUN-2006 by Mati Meron.  Added keyword IND.
;		Modified 10-APR-2008 by Mati Meron.  Added keywords ISIZE and WSIZE.
;		Modified 25-APR-2008 by Mati Meron.  Added keyword AMAX.
;		Modified 30-APR-2008 by Mati Meron.  Internal changes in binning.
;		Modified 5-MAY-2008 by Mati Meron.  Added keywords /NOERASE and /SHAVE.
;		Modified 10-MAY-2008 by Mati Meron.  Added keyword /NODATA.
;		Modified 20-MAY-2008 by Mati Meron.  Added keywords XOFF and YOFF.
;		Modified 30-JUN-2008 by Mati Meron.  Added keyword MARK.
;		Modified 25-JUL-2008 by Mati Meron.  Internal changes.
;		Modified 25-OCT-2008 by Mati Meron.  Internal changes.
;		Modified 10-APR-2009 by Mati Meron.  Internal changes.
;		Modified 10-JUN-2009 by Mati Meron.  Added keywords APPLY, PARAM and 
;		PPOS.
;		Modified 5-AUG-2009 by Mati Meron.	Added keyword TOPSIZ.
;		Modified 10-MAR-2010 by Mati Meron.  Added keyword /EVEN.
;		Modified 5-OCT-2010 by Mati Meron.  Added keyword RANGE.
;		Modified 30-JAN-2013 by Mati Meron.  Internal changes.
;		Modified 5-MAR-2013 by Mati Meron.  Added keyword RADIUS.
;		Modified 15-NOV-2013.  Added keywords GRID, GSTYLE and EXPAD.
;-

	on_error, 1

	whi = One_of(imag,cont,surf,/noz) > 0
	if Isnum(ord,/int) then word = abs(ord mod 2) else word = !order

	wimg = reform(img)
	siz = size(wimg)
	case siz[0] of
		2	:	begin
					if (size(x))[0] eq 3 then begin
						wx = reform(x[0,*,*])
						wy = reform(x[1,*,*])
					endif else begin
						if n_elements(x) lt 2 then wx = lindgen(siz[1]) $
						else wx = reform(x)
						if n_elements(y) lt 2 then wy = lindgen(siz[2]) $
						else wy = reform(y)
					endelse
				end
		3	:	begin
					imind = 2 > Default(ind,2) < (siz[1] - 1)
					wx = reform(wimg[0,*,*])
					wy = reform(wimg[1,*,*])
					wimg = reform(wimg[imind,*,*])
					siz = size(wimg)
				end
		else:	message, 'Not an image!'
	endcase
	if (size(wx))[0] eq 2 then wx = reform(wx[*,0])
	if (size(wy))[0] eq 2 then wy = reform(wy[0,*])
	if Isnum(xof) and (size(xof))[0] eq 0 then wx = wx + xof
	if Isnum(yof) and (size(yof))[0] eq 0 then wy = wy + yof
	if keyword_set(xrv) then wx = reverse(wx)
	if keyword_set(yrv) xor word then wy = reverse(wy)
	if not Arreq([n_elements(wx),n_elements(wy)],siz[1:2]) $
	then message, 'Data sizes discrepancy!'

	if Isnum(tps,/int) and not Isnum(bin,/int) then begin
		wtps = ([tps,tps[0]])[0:1]
		if Arreq(wtps,[1,1]) then wtps = [512l,512l]
		bin = ceil(1.*siz[1:2]/wtps)
	endif
	if Isnum(bin,/int) then begin
		wbin = ([bin,bin[0]])[0:1]
		dims = siz[1:2]/wbin
		rsiz = dims*wbin
		wimg = rebin(wbin[0]*wbin[1]*wimg[0:rsiz[0]-1,0:rsiz[1]-1],dims)
		if keyword_set(ave) then wimg = wimg/(wbin[0]*wbin[1])
		wx = rebin(wx[0:rsiz[0]-1],dims[0])
		wy = rebin(wy[0:rsiz[1]-1],dims[1])
		siz = size(wimg)
	endif else wbin = [1,1]

	pixfl = keyword_set(pix)
	if keyword_set(auz) and not Isnum(zom) then begin
		auz = ([auz,auz])[0:1]
		if min(auz) gt 1 then ddim = auz else ddim = [512l,512l]
		zom = ((ddim + pixfl - 1)/(siz[1:2] + pixfl - 1) > 1)
		auzfl = 1
	endif else auzfl = 0
	if Isnum(zom,/int) and whi eq 0 then begin
		wzom = ([zom,zom[0]])[0:1]
		dims = siz[1:2]*wzom
		lims = dims - wzom
		wimg = rebin(wimg,dims,sample=pixfl)
		wx = rebin(wx,dims[0])
		wy = rebin(wy,dims[1])
		if not pixfl then begin
			wimg = (wimg)[0:lims[0],0:lims[1]]
			wx = (wx)[0:lims[0]]
			wy = (wy)[0:lims[1]]
		endif
		siz = size(wimg)
	endif else wzom = [1,1]

	if Isnum(rot,/int) then begin
		wrot = rot mod 8
		if wrot lt 0 then wrot = wrot + 8
		wimg = rotate(wimg,wrot)
		wx = reform(rotate(wx,wrot))
		wy = reform(rotate(transpose(wy),wrot))
		if ((wrot + (wrot ge 4)) mod 2) then begin
			tem = wx
			wx = wy
			wy = tem
			exi = Wherinstruct('xti',_e)
			eyi = Wherinstruct('yti',_e)
			if exi ge 0 and eyi ge 0 then begin
				tem = _e.(exi)
				_e.(exi) = _e.(eyi)
				_e.(eyi) = tem
			endif
			if word and whi eq 0 then begin
				wx = reverse(wx)
				wy = reverse(wy)
			endif
		endif
		siz = size(wimg)
	endif

	if Type(fun) eq 7 then begin
		if n_elements(par) gt 0 $
		then wimg = FPU_fix(call_function(fun,wimg,par,_extra=_e)) $
		else wimg = FPU_fix(call_function(fun,wimg,_extra=_e))
	endif

	imax = max(wimg,min=imin)
	if keyword_set(amx) then begin
		imx = 1.*amx/imax
		imax = imax > amx
		wimg[0,siz[2]-1] = amx
	endif
	cmin = imin < 0
	wmin = (cmin + (0 > Default(imn,0.,/dtyp) < 1)*(imax - cmin)) > imin
	wmax = (cmin + (0 > Default(imx,1.,/dtyp) < 1)*(imax - cmin)) > wmin
	wimg = wimg > wmin
	if keyword_set(blk) then begin
		dum = where(wimg gt wmax, ndum)
		if ndum gt 0 then wimg[dum] = wmin
	endif else wimg = wimg < wmax

	lfl = keyword_set(log)
	if lfl then begin
		if keyword_set(itp) or Isnum(wimg,/int) then wimg = long(wimg) + 1 $
		else wimg = wimg > Toler(wimg)*abs(wmax)
		if whi ne 2 then wimg = alog10(wimg)
		wmax = max(wimg,min=wmin)
	endif
	owin = !d.window
	if Isnum(win,/int) then wset, win
	if keyword_set(new) and whi gt 0 then window, /free
	case whi of
		0:	begin
				if Isnum(wx,/int) and Isnum(wy,/int) then begin
					xrn = [wx[0],wx[siz[1]-1] + pixfl*wbin[0]]
					yrn = [wy[0],wy[siz[2]-1] + pixfl*wbin[1]]
				endif else begin
					xrn = [wx[0],wx[siz[1]-1] + $
					pixfl*Fltround(wzom[0]*(wx[1]-wx[0]),dig=4)]
					yrn = [wy[0],wy[siz[2]-1] + $
					pixfl*Fltround(wzom[1]*(wy[1]-wy[0]),dig=4)]
				endelse
				if lfl then zrn = 10^[wmin,wmax] else zrn = [wmin,wmax]
				fulfl = 1 - keyword_set(cln)
				if keyword_set(shv) then pscal = 3 else pscal = 4
				lpad = 16l*pscal*fulfl
				rpad = 6l*pscal*fulfl
				ssiz = siz
				isi = ssiz[1:2]
				xsiz = ssiz[1] + 2*(lpad + rpad)
				ysiz = ssiz[2] + lpad + rpad
				if Isnum(epd) then begin
					wepd = ([epd,epd])[0:1]
					xsiz = xsiz + wepd[0]
					ysiz = ysiz + wepd[1]
				endif
				if keyword_set(eve) then begin
					xsiz = (xsiz + 1)/2*2
					ysiz = (ysiz + 1)/2*2
				endif
				wsi = [xsiz,ysiz]
				if keyword_set(nod) then break

				if keyword_set(new) or !d.x_size lt xsiz or !d.y_size lt ysiz $
				then window, /free, xsiz = xsiz, ysiz = ysiz
				mpof = [!d.x_size - xsiz, !d.y_size - ysiz]
				if Isnum(pof) then wpof = 0 > ([pof,pof[0]])[0:1] < mpof $
				else wpof = [0,0]
				wpad = lpad + wpof

				if Isnum(mrk) then begin
					if n_elements(mrk) mod 4 eq 0 then begin
						if not Arreq(mrk,[0,0,0,0]) then begin
							imrk = n_elements(mrk)/4
							wmrk = reform(mrk,4,imrk)
						endif else imrk = 0
					endif else message, 'Mark size must be divisible by 4!'
				endif else imrk = 0

				plvar_keep, act = 'sav'
				if not keyword_set(ner) then erase
				device, deco = 0
				tvscl, wimg, wpad[0], wpad[1], order = word

				if fulfl then begin
					nori = Wherinstruct('nor',_e)
					if nori ge 0 then begin
						nkeep = _e.(nori)
						_e.(nori) = 0
					endif
					pps = [wpad,wpad + siz[1:2] - 1]
					ran = [xrn[0],yrn[0],xrn[1],yrn[1]]
					gst = Default(gst,1,/dtyp)
					device, deco = 1
					if keyword_set(gri) then plot, wx, wy, xran=xrn, yran=yrn, $
					/nodata, /noerase, /device, xstyle=9, ystyle=9, ticklen=1, $
					xminor= 1, yminor= 1, xgrid= gst, ygrid= gst, $
					color= !pcol.white, posit=pps, _extra=_e
					plot, wx, wy, xran=xrn, yran=yrn,/nodata,/noerase,/device, $
					xstyle= 9, ystyle= 9, ticklen= -0.01, posit= pps, _extra= _e
					if imrk gt 0 then for i = 0, imrk-1 do Rectan, radius= rad,$
					xli= wmrk[[0,2],i],yli= wmrk[[1,3],i], col=!pcol.white
					device, deco = 0
					cwid = 2*rpad/3
					carr = bytarr(cwid,siz[2])
					fill = 256l*lindgen(siz[2])/siz[2]
					if word then fill = reverse(fill)
					for i = 0l, cwid - 1 do carr[i,*] = fill
					tvscl, carr, wpad[0]+ siz[1]+ rpad/3, wpad[1], order = word
					plot, zrn, zrn, yran = zrn, /nodata, /noerase, /device, $
					posit= [wpad +[siz[1]+rpad/3,0],wpad+siz[1:2]+[rpad,0]-1], $
					xstyle = 5, ystyle = 5, ylog = lfl
					exi = Wherinstruct('xti',_e)
					if exi ge 0 then _e.(exi) = ''
					eyi = Wherinstruct('yti',_e)
					if eyi ge 0 then _e.(eyi) = ''
					axis, yax=1, ticklen=-0.01*siz[1]/cwid, ystyle=9 ,_extra=_e
					if nori ge 0 then _e.(nori) = nkeep
				endif
				plvar_keep, act = 'res'
			end
		1:	begin
				if lfl then begin
					top = floor(wmax)
					bot = floor(wmin) < 0
					dtp = top - bot
					finfl = keyword_set(fin)
					if finfl then row = [1,2,5] else row = [1,3]
					tem = findgen(2+finfl,dtp+1)
					for i = 0, dtp do tem[*,i] = row*10.^(i+bot)
					tem = tem[*]
					lev = alog10(tem)
					ntem = n_elements(tem)
					annot= string(tem)
					ftem = floor(lev)
					form = strcompress($
					'(f'+ string(abs(ftem)+3)+ '.'+ string(-ftem>1)+')',/rem)
					annot = strarr(ntem)
					for i = 0l, ntem-1 do annot[i] = string(tem[i],form=form[i])
				endif else begin
					lmax = 10.^floor(alog10(wmax))
					hmax = lmax*ceil(wmax/lmax)
					inlev = Wherinstruct('nlev',_e)
					if inlev ge 0 then nlev = _e.(inlev) else nlev = 11
					lev = hmax/10.*findgen(nlev)
				endelse
				contour, wimg, wx, wy, /fol, lev = lev, c_annot = annot, $
				xstyle=1, ystyle=1, _extra = _e
			end
		2:	begin
				shade_surf, wimg, wx, wy, zlog = lfl, charsiz= 1.4, _extra = _e
			end
	endcase
	if keyword_set(rst) then wset, owin

	return
end