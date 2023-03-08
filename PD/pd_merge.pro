Pro PD_merge, gid_dat= gdat, pin_dat= pdat, interactive = int, overlap = ovr, $
	result = res, z_result = z_res, _extra = _e
	
;+
; NAME:
;		PD_MERGE
; VERSION:
;		8.31
; PURPOSE:
;		Merging GID and Pinhole data.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		PD_MERGE, GID_DAT = GDAT, PIN_DAT = PDAT [, keywords])
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
;	/INTERACTIVE
;		Switch.  If set, the user can interactively adjust the limits on the 
;		Pinhole data to be used, as well as the extent of the overlap between 
;		the GID and Pinhole data.  The adjustement can be repeated, as many 
;		times as needed.
;	OVERLAP
;		Integer, then number of GID_DAT overlap points with PIN_DAT, on each
;		side.  If the number is too large, it'll be lowered.  Default value is 3
;	RESULT
;		Optional output, see below.
;	Z_RESULT
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to imbedded
;		routines.  Not to be used directly.
; OUTPUTS:
;		Primary output to screen only.  Additionally, optional outputs through
;		keywords.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the merged 2D data.
;	Z_RESULT
;		Returns the integrated (over Q_z) data.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The GID and Pinhole data must have some overlap, both in the horizontal
;		and the vertical dimension.
; PROCEDURE:
; 		Calls PD_GIPI_MERGE to perform the merging and displays the two input
; 		data sets and the merged result to the screen.  Calls PD_GID_ZINT.
; 		Calls IMG_REFORM from SPEC.  Calls DEFAULT, DISPLAY_MM, FPU_FIX, 
; 		MAKE_GRID, STREQ and WHERINSTRUCT, from MIDL.  
; MODIFICATION HISTORY:
;		Created 10_JUN-2014 by Mati Meron.
;-

	on_error, 1
	wnum = 20l
	hv = [256l,512l]
	bsiz = 32l
	posib = [' ','y','n']

	if (size(gdat,/dim))[0] ne 4 then message, 'Bad GID data!'
	if (size(pdat,/dim))[0] ne 4 then message, 'Bad Pinhole data!'
	
	if keyword_set(int) then done = 0 else done = 1
	repeat begin
		hgran = [min(gdat[0,*,0],max=max),max]
		vgran = [min(gdat[1,0,*],max=max),max]
		hpran = [min(pdat[0,*,0],max=max),max]
		vpran = [min(pdat[1,0,*],max=max),max]
		lim = Default(lim,hpran)
		lim = [lim[0] > hgran[0], lim[1] < hgran[1]]
		pspan = reform(pdat[0,*,0])
		dum = where(pspan ge lim[0] and pspan le lim[1],ndum)
		if ndum le 1 then begin
			wdat = pdat
			message, 'Impossible limits', /con	
		endif else wdat = pdat[*,dum,*]
		hwran = [min(wdat[0,*,0],max=max),max]
		vwran = [min(wdat[1,0,*],max=max),max]
		mdat = PD_gipi_merge(gid=gdat,pin=wdat,over=ovr)
		hmran = [min(mdat[0,*,0],max=max),max]
		vmran = [min(mdat[1,0,*],max=max),max]

		dgdat = Img_reform(gdat,$
		xval=Make_grid(hgran,hv[0]),yval=Make_grid(vgran,hv[1]))
		phv = ceil([hv[0]*(hpran[1]-hpran[0])/(hgran[1]-hgran[0]),$
					hv[1]*(vpran[1]-vpran[0])/(vgran[1]-vgran[0])])
		dpdat = Img_reform(pdat,$
		xval=Make_grid(hpran,phv[0]),yval=Make_grid(vpran,phv[1]))
		dmdat = Img_reform(mdat,$
		xval=Make_grid(hmran,hv[0]),yval=Make_grid(vmran,phv[1]))

		Display_mm, dgdat, wsi=gwsi, /shave, /nodata
		Display_mm, dpdat, wsi=pwsi, /shave, /nodata
		Display_mm, dmdat, wsi=mwsi, /shave, /nodata

		pof = [0.25,8.]*bsiz
		xsi = pof[0] + gwsi[0] + pwsi[0] + mwsi[0]
		ysi = pof[1] + gwsi[1] + bsiz/4
		if (Wherinstruct('num',_e))[0] ge 0 then ner = 1 else ner = 0
		mark = transpose([[lim],[vwran]])
		window, wnum, xsiz = xsi, ysiz = ysi, $
		tit = strcompress('IDL ' + string(wnum) + ' : PD Merge')

		Display_mm, dgdat, poff= pof, xtit= 'Q!dxy!n', ytit= 'Q!dz!n', $
		tit= 'GID Data', /shave, isi= isi, _extra= _e		
		pos = [pof[0],0,pof[0]+isi[0]-1,-1] + [1.5,1.5,1.5,7.5]*bsiz
		PD_GID_zint, gdat, /bare, noerr=ner, /noerase, /nofile, ytit='Counts', $
		title= 'Vertically integrated', position= pos, /dev, _extra= _e

		pof = pof + [gwsi[0],0]
		Display_mm, dpdat, poff= pof, /noerase, xtit= 'Q!dxy!n',ytit= 'Q!dz!n',$
		tit= 'Pinhole Data', mark= mark, /shave, isi= isi, _extra= _e
		pos = [pof[0],0,pof[0]+isi[0]-1,-1] + [1.5,1.5,1.5,7.5]*bsiz
		PD_GID_zint, pdat, /bare, noerr=ner, /noerase, /nofile, ytit='Counts', $
		title ='Vertically integrated', position= pos, /dev,_extra= _e

		pof = pof + [pwsi[0],0]
		Display_mm, dmdat, poff= pof, /noerase, xtit= 'Q!dxy!n',ytit= 'Q!dz!n',$
		tit= 'Merged Data', /shave, isi= isi, _extra= _e
		pos = [pof[0],0,pof[0]+isi[0]-1,-1] + [1.5,1.5,1.5,7.5]*bsiz
		PD_GID_zint, mdat, /bare, noerr=ner, /noerase, /nofil , ytit='Counts', $
		title ='Vertically integrated', position=pos, /dev, res= z_res,_extra=_e

		if not done then begin
			change = 0
			ovr = Default(ovr,3)
			wait, 0.001
			print
			que = ''
			read, que, prompt = 'New limits (Y/N)? '
			if Streq(que,'y',1) then begin
				read, lim, prompt = 'Enter lower_limit, upper limit:	'
				change = 1
			endif 
			read, que, prompt = 'New overlap (Y/N)? '
			if Streq(que,'y',1) then begin
				read, ovr, prompt = 'Enter new overlap value:	'
				change = 1
			endif
			done = not change
		endif
	endrep until done

	res = FPU_fix(mdat)
	return
end