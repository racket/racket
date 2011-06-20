#lang scribble/doc
@(require "common.rkt")

@gametitle["Paint By Numbers" "paint-by-numbers" "Logic Game"]

The object of @game{Paint By Numbers} is to discover which cells
should be colored blue and which should be colored white.  Initially,
all squares are grey, indicating that the correct colors are not
known.  The lists of numbers to the left and above the grid are your
clues to the correct color of each square.  Each list of numbers
specifies the pattern of blue squares in the row beside it or the
column below it.  Each number indicates the length of a group of blue
squares.  For example, if the list of numbers beside the first row is
@onscreen{2 3} then you know that there is a contiguous block of two
blue squares followed by a contiguous block of three blue squares with
at least one white square between them.  The label does not tell you
where the blue squares are, only their shapes.  The trick is to gather
as much information as you can about each row, and then use that
information to determine more about each column.  Eventually you
should be able to fill in the entire puzzle.

Click on a square to toggle it between blue and gray.  Hold down a
modifier key (shift, command, meta, or alt depending on the platform)
to toggle a square between white and gray.  The third button under
unix and the right button under windows also toggles between white and
gray.

For some puzzles, hints are available.  Choose the @menuitem["Nongram"
"Show Mistakes"] menu item to receive the hints.  This will turn all
incorrectly colored squares red.

Thanks to Shoichiro Hattori for his puzzles!  Visit him on the web at:

@centerline{@selflink["http://hattori.m78.com/puzzle/"]}

Thanks also to many of the contributors to the Kajitani web site for
permission to re-distribute their puzzles.  Visit them online at:

@centerline{@selflink[;"http://www02.so-net.ne.jp/~kajitani/index.html"
                      "http://nonogram.freehostia.com/pbn/index.html"]}

The specific contributors who have permitted their puzzles to be
redistributed are:

@verbatim[#:indent 2]{
 snordmey /at/ dayton <dot> net
 jtraub /at/ dragoncat <dot> net
 e0gb258s /at/ mail <dot> erin <dot> utoronto <dot> ca
 mattingly /at/ bigfoot <dot> com
 jennifer <dot> forman /at/ umb <dot> edu
 karen <dot> hoover /at/ bigfoot <dot> com
 sssstree /at/ ix <dot> netcom <dot> com
 we_bakers_3 /at/ earthlink <dot> net
 bbart /at/ cs <dot> sfu <dot> ca
 jonesjk /at/ thegrid <dot> net
 rrichard /at/ lexitech <dot> ca
 helena <dot> montauban /at/ auroraenergy <dot> com <dot> au
 barblane /at/ ionsys <dot> com
 m5rammy /at/ maale5 <dot> com
 nmbauer /at/ sprynet <dot> com
 ncfrench /at/ aol <dot> com
 km29 /at/ drexel <dot> edu
 jjl /at/ stanford <dot> edu
 disneyfan13 /at/ hotmail <dot> com
 richard /at/ condor-post <dot> com
 lady_tabitha /at/ yahoo <dot> com
 vaa /at/ psulias <dot> psu <dot> edu
 kimbhall /at/ yahoo <dot> com
 kcottam /at/ cusa <dot> com
 karganov /at/ hotmail <dot> com
 jdmaynard /at/ excite <dot> com
 mnemoy /at/ gameworks <dot> com
 arrelless /at/ jayco <dot> net
 azisi /at/ skiathos <dot> physics <dot> auth <dot> gr
 whoaleo /at/ hotmail <dot> com
 tucker1999 /at/ earthlink <dot> net
 bergles /at/ yahoo <dot> com
 elisabeth <dot> springfelter /at/ lanab <dot> amv <dot> se
 ewhaynes /at/ mit <dot> edu
 mjcarroll /at/ ccnmail <dot> com
 dahu /at/ netcourrier <dot> com
 joy /at/ dcs <dot> gla <dot> ac <dot> uk
 piobst /at/ wam <dot> umd <dot> edu
 dani681 /at/ aol <dot> com
 Talzhemir <pixel /at/ realtime <dot> net>
 hkittredge /at/ hotmail <dot> com
 allraft /at/ sccoast <dot> net
 karlvonl /at/ geocities <dot> com
 ailsa /at/ worldonline <dot> nl
 Carey Willis <N8NRG /at/ hotmail <dot> com>
 citragreen /at/ hotmail <dot> com
 dhalayko /at/ cgocable <dot> net
 jontive1 /at/ elp <dot> rr <dot> com
 hublan /at/ rocketmail <dot> com
 barbridgway /at/ compuserve <dot> com
 mijoy /at/ mailcity <dot> com
 joostdh /at/ sci <dot> kun <dot> nl
 gossamer_kwaj /at/ hotmail <dot> com
 williamson /at/ proaxis <dot> com
 vacko_6 /at/ hotmail <dot> com
 jojess /at/ earthlink <dot> net
}
