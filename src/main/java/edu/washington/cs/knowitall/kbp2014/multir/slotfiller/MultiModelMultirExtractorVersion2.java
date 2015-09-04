package edu.washington.cs.knowitall.kbp2014.multir.slotfiller;

//import edu.washington.multirframework.argumentidentification.NERArgumentIdentificationPlusMISC;
//import edu.washington.multirframework.argumentidentification.ExtendedNERArgumentIdentification;
import edu.washington.multirframework.featuregeneration.DefaultFeatureGenerator;
import edu.washington.multirframework.argumentidentification.*;

public class MultiModelMultirExtractorVersion2 extends MultiModelMultirExtractor {

	public MultiModelMultirExtractorVersion2(){

		super();
	
		fg = new DefaultFeatureGenerator();
        // NERArgumentIdentificationPlusMISC used for KBP2014 official run
		//ai = NERArgumentIdentificationPlusMISC.getInstance();
		//ai = ExtendedNERArgumentIdentification.getInstance();
		ai = KBP_NELAndNERArgumentIdentification.getInstance();
		
		sigs.add(FigerAndNERTypeSignatureORGDATESententialInstanceGeneration.getInstance());
		sigs.add(FigerAndNERTypeSignatureORGLOCSententialInstanceGeneration.getInstance());
		sigs.add(FigerAndNERTypeSignatureORGNUMSententialInstanceGeneration.getInstance());
		sigs.add(FigerAndNERTypeSignatureORGORGSententialInstanceGeneration.getInstance());
		sigs.add(FigerAndNERTypeSignatureORGOTHERSententialInstanceGeneration.getInstance());
		sigs.add(FigerAndNERTypeSignatureORGPERSententialInstanceGeneration.getInstance());
		
		sigs.add(FigerAndNERTypeSignaturePERDATESententialInstanceGeneration.getInstance());
		sigs.add(FigerAndNERTypeSignaturePERLOCSententialInstanceGeneration.getInstance());
		sigs.add(FigerAndNERTypeSignaturePERNUMSententialInstanceGeneration.getInstance());
		sigs.add(FigerAndNERTypeSignaturePERORGSententialInstanceGeneration.getInstance());
		sigs.add(FigerAndNERTypeSignaturePEROTHERSententialInstanceGeneration.getInstance());
		sigs.add(FigerAndNERTypeSignaturePERPERSententialInstanceGeneration.getInstance());
		
		modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/ORGDATE-Model");	
		modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/ORGLOC-Model");	
		modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/ORGNUM-Model");	
		modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/ORGORG-Model");	
		modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/ORGOTHER-Model");	
		modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/ORGPER-Model");	
		
		modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/PERDATE-Model");
		modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/PERLOC-Model");
		modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/PERNUM-Model");
		modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/PERORG-Model");
		modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/PEROTHER-Model");
		modelFilePaths.add("/projects/WebWare6/Multir/KBP2014/KBPModel1/Models/PERPER-Model");
		
	}
	
	
}
