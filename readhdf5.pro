;NAME:        readhdf5.pro (BETA)
;             (Note: The data acquisition system is currently under development. Parts of
;             this code may be re-written or updated from time to time.)
;
;AUTHOR:      swjtang / Timothy DeHaas
;DATE:        01 Mar 2018
;DESCRIPTION: This is a translator used to convert hdf5 files into an IDL readable format.
;             It is written in the form of a tutorial and uses a number of procedures that
;             were written by past UCLA grad students.
;
;SYNTAX:      RESULT = READHDF5(Filename, XPOS=XPOS, YPOS=YPOS, TIME=TIME)
;             RESULT   =  (Output) A floating array with dimensions [Channel,Time,XPos,YPos]
;             FILENAME =  (Input) A string which specifies the filepath where the hdf5 file is
;                         stored on the computer.
;             The following are optional keywords. They specify the variable in which a specific
;             quantity is stored:
;             XPOS     =  A floating vector containing all the X-positions (1st coordinate).
;             YPOS     =  A floating vector containing all the Y-positions (2nd coordinate).
;             TIME     =  A floating vector containing all the time values.

FUNCTION READHDF5, FILENAME, XPOS=XPOS, YPOS=YPOS, TIME=TIME

;Opening a file is easy. One need only use a program called "h5restore.pro" However,
;the proper syntax is needed. HDF5 files are structured files. That is, they are like
;a folder in your computer. Data of any type can be stored under this folder or
;another "subfolder". In order to retrieve data from the hdf5 file, you need to specify
;the folder where the data is located. In 180E, the data is stored in two types
;of "sub-folders" or "groups". One stores the positions, the other stores the data.
;The two folders are as follows:
    DATA_GROUP     = "/Acquisition/LeCroy_scope/"
    POSITION_GROUP = "/Control/Positions/"
    
;We will read in the Data twice. First will be the positions. These positions will
;be stored in XPos (first coordinate) and YPos (second coordinate). They can also 
;refer to zpos (first coordinate) and theta (second coordinate).

  ;Reading in the Positions
   H5Restore, Filename=Filename, Group = POSITION_GROUP

;Now, we could certainly analyze the data without really knowing the position. But
;for the sake of generality, we will try to write this program to analyze the positions
;as well. The positions that are read in are in a type of data called a "structure."
;A structure is an IDL object, just like an hdf5 file, that can store different
;types of data in one IDL variable. The positions that are read in are actually an
;array of structures. This is quite complicated. The variable is called:
;   "positions_setup_array"
;
;Every time the data acquisition system records data, it adds a structure to the 
;variable above. At the end, if I would like to know what the positions are for the
;first point measured, we would print:
;   PRINT, positions_setup_array[0].x
;   PRINT, positions_setup_array[0].y
;In other words, the code above would print the first measured x-position and then
;the first measured y-position.

 ;Rename this very long IDL variable of positions
   POS_STRCT = positions_setup_array

 ;Finds the total number of measurements taken
   PS_tot = Max(POS_STRCT.Line_number)

 ;Determines the number of shots per position by finding the total number of points with
 ; the same coordinate as the first data point.
   nshots = 1
   WHILE POS_STRCT[nshots].x EQ POS_STRCT[0].x AND POS_STRCT[nshots].y EQ POS_STRCT[0].y DO BEGIN
   	nshots++
   ENDWHILE

 ;Determines how many x-positions there are for one y-coordinate. We first count the number of
 ; times the same y-position occurs, then divide by the number of shots per position to get 
 ; the number of unique x-positions
   dX = Size(WHERE (POS_STRCT.y EQ POS_STRCT[0].y), /Dimensions) / nshots
   dX = dX[0]		; in case there is more than one
   dY = PS_tot[0]/(dX*nshots)

 ;The data is stored in a long 1D array, but we are interested in a 2D plane of data. 
 ;Thus, we need to restructure the array. (Assumes data run is complete and PS_tot=dX*dY*nshots)
 ;The order of restructuring follows the order in which the data is taken, which is:
 ; change shots -> change X -> change Y
   YAR  = Reform(POS_STRCT.y, nshots, dX, dY) ;3D array of y-positions
   XAR  = Reform(POS_STRCT.x, nshots, dX, dY) ;3D array of x-Positions
   YPOS = REFORM(YAR[0,0,*]) ;1D vector of unique y-positions
   XPOS = REFORM(XAR[0,*,0]) ;1D vector of unique x-positions

;READ IN THE DATA
;Now, we want to read in the data that was taken per position. Remember that the
;Oscilloscope stores 4 channels of voltage data as a function of time. Because the data
;stored per channel is large, IDL imports that data as an object. We can read in
;the data from this object by using the "r()" function. See below.

;----------
  ; We first determine which channel has data. Some channels may be disabled by the
  ; data acquisition system (i.e. its blank) in order to reduce the file size. 
  ; Active channels will have "headers" written to the hdf5 file. 
  H5Restore, Filename=Filename, Group = DATA_GROUP+"/Headers"

  ;If the "header" does not exist, a simple size check will return zero. This vector
  ;will also be used to inform the rest of the program if a channel is disabled or not.
  checkzero= [(SIZE(Channel1))[0],(SIZE(Channel2))[0],(SIZE(Channel3))[0],(SIZE(Channel4))[0]]

  ;The number of the first active channel that was not disabled.
  firstnonzero=(WHERE(checkzero GT 0))[0]+1

;----------
;We now read in the actual data. h5restore restores the data (channels 1 to 4) as 
;an IDLObject. This just means that IDL now knows where the data is on the computer,
;but IDL has not imported any numbers into the program. To import the data, we do the
;code below.
  H5Restore, Filename=Filename, Group = DATA_GROUP

;Sometimes, the data is stored in a h5variable object because there is a lot of data
;packaged into one channel (e.g. when the scope sampling rate is high). In this situation, 
;the structures require an additional step to unpackage. The following checks if the
;first non-zero channel is a h5variable object and then unpackages it. We assume the rest
;of the channels are of the same type.
    CASE firstnonzero OF
      1:  ChannelSz = Size(Channel1)
      2:  ChannelSz = Size(Channel2)
      3:  ChannelSz = Size(Channel3)
      4:  ChannelSz = Size(Channel4)
    ENDCASE
    
    ; We only restore the channel if it contains data.
    IF (ChannelSz[1] EQ 11) THEN BEGIN
      IF checkzero[0] NE 0 THEN Channel1 = Channel1->r([-1,-1])
      IF checkzero[1] NE 0 THEN Channel2 = Channel2->r([-1,-1])
      IF checkzero[2] NE 0 THEN Channel3 = Channel3->r([-1,-1])
      IF checkzero[3] NE 0 THEN Channel4 = Channel4->r([-1,-1])
    ENDIF

; The imported data is stored in a huge vector, but it has to to be reformed into a 
; array with the right dimensions. We want our output array to be of the form
;   OUTPUT = [Channels, Time, x-positions, y-positions] .
; The following checks the data for its intended dimensions:
    CASE firstnonzero OF
      1:  ChannelD = Size(Channel1, /Dimensions)
      2:  ChannelD = Size(Channel2, /Dimensions)
      3:  ChannelD = Size(Channel3, /Dimensions)
      4:  ChannelD = Size(Channel4, /Dimensions)
    ENDCASE
  
   ; Create a variable to store the data.
   OUTPUT = FLTARR(4,ChannelD[0],nshots,dX,dY)
   
   ; Reforms the channel data only if it contains data
   IF checkzero[0] NE 0 THEN OUTPUT[0,*,*,*,*] = Reform(Channel1,ChannelD[0],nshots,dX,dY)
   IF checkzero[1] NE 0 THEN OUTPUT[1,*,*,*,*] = Reform(Channel2,ChannelD[0],nshots,dX,dY)
   IF checkzero[2] NE 0 THEN OUTPUT[2,*,*,*,*] = Reform(Channel3,ChannelD[0],nshots,dX,dY)
   IF checkzero[3] NE 0 THEN OUTPUT[3,*,*,*,*] = Reform(Channel4,ChannelD[0],nshots,dX,dY)

   ; Rearrange the array so that nshots dimension is at the end
   OUTPUT2 = FLTARR(4,ChannelD[0],dX,dY,nshots)
   FOR shot=0,nshots-1 DO BEGIN
		OUTPUT2[*,*,*,*,shot] = OUTPUT[*,*,shot,*,*]
   ENDFOR

RETURN, OUTPUT2
END