Function Pick_point, img, query = que, verify = ver

	on_error, 1

	ok = 1 - keyword_set(ver)
	que = Default(que,'Select location!')
	wimg = Byte_img(img,size=siz)
	dum = Consec(siz=siz,/new)
	if !d.window eq (-1) then wset
	wshow
	tvscl, wimg

	repeat begin
		print
		print, que
		cursor, x, y, /device, /down
		cent = [x,y]
		if not ok then begin
			dum = Consec(siz=siz,cent=cent,rad=2)
			wimg[dum] = wimg[dum] + 128b
			tvscl, wimg
			con = 0
			while not con do begin
				print
				print, 'Selected : ', cent
				print, 'Is this OK? Y/N'
				rep = get_kbrd(1)
				ok = Streq(rep,'y',1) or Streq(rep,string(10b),1)
				con = ok or Streq(rep,'n',1)
			endwhile
			print
			wimg(dum) = wimg[dum] + 128b
			tvscl, wimg
		endif
	endrep until ok
	print, 'OK'

	return, cent
end