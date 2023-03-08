Pro Apsu_27

	on_error, 1

	ef = [3.55,11./3,4,13./3,14./3,5,16./3,17./3,6,19./3,32./5,20./3,7,$
	22./3,8,26./3,9,28./3,10,11,12]
	ef = reverse(ef)
	
	for i = 0l, n_elements(ef)-1 do $
	un_make, 3, 0.027, !blpar.rgam, 77, 'apsu_27_s*', efir = ef[i], at=15, $
	!blpar.rsig, !blpar.asig, ang= [0.04,0.04], np= [40,40], /auto, /sho

return
end