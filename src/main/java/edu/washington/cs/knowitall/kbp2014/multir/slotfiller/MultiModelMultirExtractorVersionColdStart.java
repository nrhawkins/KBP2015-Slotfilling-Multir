package edu.washington.cs.knowitall.kbp2014.multir.slotfiller;

import edu.washington.multirframework.featuregeneration.DefaultFeatureGeneratorNoCJ;
import edu.washington.multirframework.argumentidentification.*;

public class MultiModelMultirExtractorVersionColdStart extends MultiModelMultirExtractor {

	public MultiModelMultirExtractorVersionColdStart(){

		super();
	
		fg = new DefaultFeatureGeneratorNoCJ();
        // NERArgumentIdentificationPlusMISC used for KBP2014 official run
		//ai = NERArgumentIdentificationPlusMISC.getInstance();
		//ai = ExtendedNERArgumentIdentification.getInstance();
		//ai = KBP_NELAndNERArgumentIdentification.getInstance();
		ai = PERLOCArgumentIdentification.getInstance();
		
		//sigs.add(FigerAndNERTypeSignatureORGDATESententialInstanceGeneration.getInstance());
		//sigs.add(FigerAndNERTypeSignatureORGLOCSententialInstanceGeneration.getInstance());
		//sigs.add(FigerAndNERTypeSignatureORGNUMSententialInstanceGeneration.getInstance());
		//sigs.add(FigerAndNERTypeSignatureORGORGSententialInstanceGeneration.getInstance());
		//sigs.add(FigerAndNERTypeSignatureORGOTHERSententialInstanceGeneration.getInstance());
		//sigs.add(FigerAndNERTypeSignatureORGPERSententialInstanceGeneration.getInstance());
		
		//sigs.add(FigerAndNERTypeSignaturePERDATESententialInstanceGeneration.getInstance());
		sigs.add(FigerAndNERTypeSignaturePERLOCSententialInstanceGeneration.getInstance());
		//sigs.add(FigerAndNERTypeSignaturePERNUMSententialInstanceGeneration.getInstance());
		//sigs.add(FigerAndNERTypeSignaturePERORGSententialInstanceGeneration.getInstance());
		//sigs.add(FigerAndNERTypeSignaturePEROTHERSententialInstanceGeneration.getInstance());
		//sigs.add(FigerAndNERTypeSignaturePERPERSententialInstanceGeneration.getInstance());
		
		//modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/ORGDATE-Model");	
		//modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/ORGLOC-Model");	
		//modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/ORGNUM-Model");	
		//modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/ORGORG-Model");	
		//modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/ORGOTHER-Model");	
		//modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/ORGPER-Model");	
		
		//modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/PERDATE-Model");
		//modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/PERLOC-Model");
		modelFilePaths.add("/projects/WebWare6/KBP_2015/multir/model");
		//modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/PERNUM-Model");
		//modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/PERORG-Model");
		//modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/PEROTHER-Model");
		//modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/PERPER-Model");
		
	}
	
	
}
