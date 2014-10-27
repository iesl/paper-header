# paperheader 
A CRF for tagging sections of an academic paper header based on FACTORIE

## Usage

1. Clone the repo, package the source code and dependencies into a jar:

        git clone https://github.com/iesl/paper-header.git
        cd paper-header
        sbt package
        
    The jar file will now be in `paper-header/target/scala-2.10/paperheader_2.10-1.0.jar`.

2. Now, create a folder called `lib/` at the root of your own project and copy the jar file over into it (so that SBT will 
recognize it as an "unmanaged dependency").
 
See the example project in `example/` for more info. In general, all you should need to do in order to use the CRF is:

        import edu.umass.cs.iesl.paperheader.crf._
        
        object MyProject {
            ...
            val docs = LoadTSV("path/to/some/data", false)
            val tagger = HeaderTaggerCRF
            docs.foreach(tagger.process)
            ...
        }
            

Note that the CRF expects to process data labeled with x/y coordinates and font size information. The default loader 
(`src/main/crf/LoadTSV`) expects this data in four tab-separated columns: 

        token-string    x-pos   y-pos   font-size

You can find the full data set used for training in `data/fullpaper-headers.tsv`.

## Header sections supported

* abstract
* address
* author
* date
* email
* institution
* keyword
* title

Achieves ~92% F1 on a token-by-token basis.