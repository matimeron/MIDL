Function CCD_img_read, filename = fnam, bin = bin, rot = rot, last = las, $
	show = sho, _extra = _e

	common CCD_stuff, exs, nco, nro, lfnam, extr
	on_error, 1

	exs = Default(exs,0)
	lasfl = keyword_set(las) and exs
	if lasfl then tfnam = lfnam else tfnam = Default(fnam,'',/dtyp)
	info = CCD_info_read(filename=tfnam,fullname=wfnam,_extra=_e)
	if Verify_struct(info,'ccd_info') then begin
		if not exs then begin
			!order = 1
			nco = info.head.ncols
			nro = info.head.nrows
		endif
		res = {ccd_img}
		res.fnam = (lfnam = wfnam)
		res.info = info
	endif else message, 'Some problems with the data file!'

	case info.head.npixelb of
		1	:	img =  bytarr(info.head.ncols,info.head.nrows)
		2	:	img = uintarr(info.head.ncols,info.head.nrows)
		4	:	img = ulonarr(info.head.ncols,info.head.nrows)
		else:	message, 'Unrecognizable data format, exiting!'
	endcase

	openr, icun, res.fnam, /get_lun
	off = info.blkl*info.head.hdrblks
	point_lun, icun, off
	readu, icun, img
	img = ulong(img)
	if info.head.noverfl gt 0 then begin
		off = off + info.head.npixelb*info.head.nrows*info.head.ncols
		ovdat = strarr(info.head.noverfl)
		rform = strcompress('('+string(info.head.noverfl)+'a16)',/rem)
		point_lun, icun, off
		readf, icun, ovdat, form = rform
		val = ulong(strtrim(strmid(ovdat,0,9),2))
		loc = long(strtrim(strmid(ovdat,9),2))
		img[loc] = val
	endif
	free_lun, icun

	if lasfl then dbin = extr.bin else dbin = 2
	res.extr.bin = 2l^(floor(alog(Default(bin,dbin) > 1)/alog(2)))
	res.imco = nco/res.extr.bin
	res.imro = nro/res.extr.bin
	if res.extr.bin gt 1 then img = $
	ulong(rebin(res.extr.bin^2*img,res.imco,res.imro))
	if lasfl then drot = extr.rot else drot = 0
	res.extr.rot = Default(rot,drot,/dtyp)
	if res.extr.rot ne 0 then img = rotate(img,(4 - res.extr.rot) mod 4)
	siz = size(img)
	res.imco = siz[1]
	res.imro = siz[2]
	res.xval[0:res.imco-1] = findgen(res.imco)
	res.yval[0:res.imro-1] = reverse(findgen(res.imro))
	res.img[0:res.imco-1,0:res.imro-1] = img

	if exs then begin
		extr.bin = res.extr.bin
		extr.rot = res.extr.rot
	endif else estr = res.extr
	exs = exs > 1

	if keyword_set(sho) then Display_mm, img, /yrev, _extra = _e

	return, res
end