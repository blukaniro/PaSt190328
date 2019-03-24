print ("Image", "Area", "Perimeter", "Major", "Minor", "Circularity");
dir0=getDirectory("home") ;
dir=dir0+"Grain/";
list=getFileList(dir);
for (j=0; j<139; j++) {
    open(dir +list[j]);
    run("8-bit");
    setAutoThreshold("Default");
    run("Threshold", "thresholded remaining black");
    for (i=0; i<9; i++) {
        run("Erode");
        }
    for (i=0; i<9; i++) {
        run("Dilate");
        }
    run("Set Scale...", "unit=pixel");
    run("Set Measurements...", "area perimeter fit ellipse");
    run("Analyze Particles...", "size=1500-Infinity pixel circularity=0.00-1.00 show=Nothing");
     if (nResults==0) {
        print (list[j], "NA", "NA", "NA", "NA", "NA");
        } else {
        Area=getResult("Area",0);
        Perimeter=getResult("Perim.",0);
        Major=getResult("Major",0);
        Minor=getResult("Minor",0);
        Circularity=4*3.141592*Area/(Perimeter*Perimeter);
        print (list[j], Area, Perimeter, Major, Minor, Circularity);
        run("Clear Results");
        }
        close();
    }