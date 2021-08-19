#include "Bff.h"
#include "MeshIO.h"
#include "HoleFiller.h"
#include "Generators.h"
#include "ConePlacement.h"
#include "Cutter.h"

#include <Rcpp.h>
using namespace bff;
using namespace Rcpp;

void loadModel(const std::string& inputPath, Model& model,
			   std::vector<bool>& surfaceIsClosed)
{
	std::string error;
	if (MeshIO::read(inputPath, model, error)) {
		int nMeshes = model.size();
		surfaceIsClosed.resize(nMeshes, false);

		for (int i = 0; i < nMeshes; i++) {
			Mesh& mesh = model[i];
			int nBoundaries = (int)mesh.boundaries.size();

			if (nBoundaries >= 1) {
				// mesh has boundaries
				int eulerPlusBoundaries = mesh.eulerCharacteristic() + nBoundaries;

				if (eulerPlusBoundaries == 2) {
					// fill holes if mesh has more than 1 boundary
					if (nBoundaries > 1) {
						if (HoleFiller::fill(mesh)) {
							// all holes were filled
							surfaceIsClosed[i] = true;
						}
					}

				} else {
					// mesh probably has holes and handles
					HoleFiller::fill(mesh, true);
					Generators::compute(mesh);
				}

			} else if (nBoundaries == 0) {
				if (mesh.eulerCharacteristic() == 2) {
					// mesh is closed
					surfaceIsClosed[i] = true;

				} else {
					// mesh has handles
					Generators::compute(mesh);
				}
			}
		}

	} else {
		// std::cerr << "Unable to load file: " << inputPath << ". " << error << std::endl;
		// exit(EXIT_FAILURE);
		stop("Unable to load file %s.", inputPath);
	}
}

void flatten(Model& model, const std::vector<bool>& surfaceIsClosed,
			 int nCones, bool flattenToDisk, bool mapToSphere)
{
	int nMeshes = model.size();
	for (int i = 0; i < nMeshes; i++) {
		Mesh& mesh = model[i];
		BFF bff(mesh);

		if (nCones > 0) {
			std::vector<VertexIter> cones;
			DenseMatrix coneAngles(bff.data->iN);
			int S = std::min(nCones, (int)mesh.vertices.size() - bff.data->bN);

			if (ConePlacement::findConesAndPrescribeAngles(S, cones, coneAngles, bff.data, mesh)
				== ConePlacement::ErrorCode::ok) {
				if (!surfaceIsClosed[i] || cones.size() > 0) {
					Cutter::cut(cones, mesh);
					bff.flattenWithCones(coneAngles, true);
				}
			}

		} else {
			if (surfaceIsClosed[i]) {
				if (mapToSphere) {
					bff.mapToSphere();

				} else {
					// std::cerr << "Surface is closed. Either specify nCones or mapToSphere." << std::endl;
					// exit(EXIT_FAILURE);
					stop("Surface is closed. Either specify nCones or mapToSphere.");
				}

			} else {
			  if (mapToSphere) {
			    stop("Surface is not closed, you cannot map to a sphere.");
			  } else {
  				if (flattenToDisk) {
  					bff.flattenToDisk();
  
  				} else {
  					DenseMatrix u(bff.data->bN);
  					bff.flatten(u, true);
  				}
			  }
			}
		}
	}
}

void flattenToShape(Model& model, const std::vector<bool>& surfaceIsClosed,
             std::vector<bff::Vector>& gamma)
{
  int nMeshes = model.size();
  for (int i = 0; i < nMeshes; i++) {
    Mesh& mesh = model[i];
    BFF bff(mesh);
    
    if (surfaceIsClosed[i]) {
          // std::cerr << "Surface is closed. Either specify nCones or mapToSphere." << std::endl;
          // exit(EXIT_FAILURE);
          stop("Surface is closed. Flattening to a target boundary is not available. Try auto_flatten instead.");
        
    } else {
        
          bff.flattenToShape(gamma);
      
    }
  }
      
}

void writeModelUVs(const std::string& outputPath, Model& model,
				   const std::vector<bool>& surfaceIsClosed, bool mapToSphere,
				   bool normalizeUVs)
{
	int nMeshes = model.size();
	std::vector<bool> mappedToSphere(nMeshes, false);
	for (int i = 0; i < nMeshes; i++) {
		if (surfaceIsClosed[i]) {
			mappedToSphere[i] = mapToSphere;
		}
	}

	if (!MeshIO::write(outputPath, model, mappedToSphere, normalizeUVs)) {
		// std::cerr << "Unable to write file: " << outputPath << std::endl;
		// exit(EXIT_FAILURE);
		stop("Unable to write file: %s", outputPath);
	}
}

// [[Rcpp::export]]
void auto_flatten(const std::string& inputPath,
                  int nCones, bool flattenToDisk, bool mapToSphere,
                  bool normalizeUVs,
                  const std::string& outputPath) {

  // load model
  Model model;
  std::vector<bool> surfaceIsClosed;
  loadModel(inputPath, model, surfaceIsClosed);
  
  // set nCones to 8 for closed surfaces`
  for (int i = 0; i < model.size(); i++) {
    if (surfaceIsClosed[i] && !mapToSphere && nCones < 3) {
      std::cout << "Setting nCones to 8." << std::endl;
      nCones = 8;
    }
  }
  
  // flatten
  flatten(model, surfaceIsClosed, nCones, flattenToDisk, mapToSphere);
  
  // write model uvs to output path
  writeModelUVs(outputPath, model, surfaceIsClosed, mapToSphere, normalizeUVs);
  
}

// [[Rcpp::export]]
void flatten_to_shape(const std::string& inputPath,
                     NumericMatrix boundary_shape,
                     bool normalizeUVs,
                     const std::string& outputPath) {
  
  // put boundary into correct format
  int nB = boundary_shape.nrow();
  std::vector<bff::Vector> gamma(nB);
  for (int i = 0; i < nB; i++) {
    gamma[i] = bff::Vector(boundary_shape(i, 0), boundary_shape(i, 1)); 
  }
    
  // load model
  Model model;
  std::vector<bool> surfaceIsClosed;
  loadModel(inputPath, model, surfaceIsClosed);
  
  // flatten
  flattenToShape(model, surfaceIsClosed, gamma);
  
  // write model uvs to output path
  bool mapToSphere = false;
  writeModelUVs(outputPath, model, surfaceIsClosed, mapToSphere, normalizeUVs);

}
