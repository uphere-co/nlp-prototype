#include "SVM.h"

namespace tfkld{
namespace svm{

static const char *solver_type_table[]=
{
	"L2R_LR", "L2R_L2LOSS_SVC_DUAL", "L2R_L2LOSS_SVC", "L2R_L1LOSS_SVC_DUAL", "MCSVM_CS",
	"L1R_L2LOSS_SVC", "L1R_LR", "L2R_LR_DUAL",
	"", "", "",
	"L2R_L2LOSS_SVR", "L2R_L2LOSS_SVR_DUAL", "L2R_L1LOSS_SVR_DUAL", NULL
};


namespace training{        
static char *line = NULL;
static int max_line_len;

struct feature_node *x_space;
struct parameter param;
struct problem prob;
struct model* model_;
int flag_cross_validation;
int flag_find_C;
int flag_C_specified;
int flag_solver_specified;
int nr_fold;
double bias;

void print_null(const char *s) {}
    
struct mParam* Do_Train(std::vector<std::string> &tag, std::vector<std::vector<float>> &svec) {
    char *cargv[100];
    int cargc;
    
    cargv[0] = (char *)"./test";
    cargv[1] = (char *)"test_train.txt";
    cargv[2] = (char *)"KLD.model";
    cargc = 3;

	char input_file_name[1024];
	char model_file_name[1024];

    mParam *mparams;
    mparams = Malloc(mParam, 1);
    
    std::string solver_type;
    int nr_class;
    int *label;
    int nr_feature;
    double bias;
    double *w;
    int n;
    int w_size;
    int nr_w;
    
	const char *error_msg;

	parse_command_line(cargc, cargv, input_file_name, model_file_name);
	read_problem_mem(tag,svec);
    error_msg = check_parameter(&prob,&param);


	if(error_msg)
	{
		fprintf(stderr,"ERROR: %s\n",error_msg);
		exit(1);
	}

	if (flag_find_C)
	{
		do_find_parameter_C();
	}
	else if(flag_cross_validation)
	{
		do_cross_validation();
	}
	else
	{
		model_=train(&prob, &param);

        const parameter& param = model_ -> param;
        nr_feature=model_->nr_feature;
        
        if(model_->bias>=0)
            n=nr_feature+1;
        else
            n=nr_feature;

        w_size = n;
        if(model_->nr_class==2 && model_->param.solver_type != MCSVM_CS)
            nr_w=1;
        else
            nr_w=model_->nr_class;
        
        solver_type = solver_type_table[param.solver_type];
        nr_class = model_ -> nr_class;
        label = Malloc(int,nr_class);
        for(int q=0; q<nr_class;q++)
            label[q] = model_->label[q];
        bias=model_->bias;
        
        w = Malloc(double, w_size*nr_w + nr_w);
        for(int q=0; q<w_size; q++)
        {
            for(int p=0; p<nr_w; p++)
                w[q*nr_w+p]=model_->w[q*nr_w+p];
        }
        
		if(save_model(model_file_name, model_))
		{
			fprintf(stderr,"can't save model to file %s\n",model_file_name);
			exit(1);
		}
		free_and_destroy_model(&model_);
	}
	destroy_param(&param);
	free(prob.y);
	free(prob.x);
	free(x_space);
	free(line);

    mparams -> solver_type = solver_type;
    mparams -> nr_class = nr_class;
    mparams -> nr_feature = nr_feature;
    mparams -> bias = bias;
    
    mparams -> label = Malloc(int,nr_class);
    mparams -> w = Malloc(double, w_size*nr_w + nr_w);
    
    for(int q=0; q<nr_class;q++) mparams -> label[q] = label[q];
    for(int q=0; q<w_size; q++)
    {
        for(int p=0; p<nr_w; p++)
            mparams -> w[q*nr_w+p] = w[q*nr_w+p];
    }
    
    return mparams;
}

void do_find_parameter_C()
{
	double start_C, best_C, best_rate;
	double max_C = 1024;
	if (flag_C_specified)
		start_C = param.C;
	else
		start_C = -1.0;
	printf("Doing parameter search with %d-fold cross validation.\n", nr_fold);
	find_parameter_C(&prob, &param, nr_fold, start_C, max_C, &best_C, &best_rate);
	printf("Best C = %g  CV accuracy = %g%%\n", best_C, 100.0*best_rate);
}

void do_cross_validation()
{
	int i;
	int total_correct = 0;
	double total_error = 0;
	double sumv = 0, sumy = 0, sumvv = 0, sumyy = 0, sumvy = 0;
	double *target = Malloc(double, prob.l);

	cross_validation(&prob,&param,nr_fold,target);
	if(param.solver_type == L2R_L2LOSS_SVR ||
	   param.solver_type == L2R_L1LOSS_SVR_DUAL ||
	   param.solver_type == L2R_L2LOSS_SVR_DUAL)
	{
		for(i=0;i<prob.l;i++)
		{
			double y = prob.y[i];
			double v = target[i];
			total_error += (v-y)*(v-y);
			sumv += v;
			sumy += y;
			sumvv += v*v;
			sumyy += y*y;
			sumvy += v*y;
		}
		printf("Cross Validation Mean squared error = %g\n",total_error/prob.l);
		printf("Cross Validation Squared correlation coefficient = %g\n",
				((prob.l*sumvy-sumv*sumy)*(prob.l*sumvy-sumv*sumy))/
				((prob.l*sumvv-sumv*sumv)*(prob.l*sumyy-sumy*sumy))
			  );
	}
	else
	{
		for(i=0;i<prob.l;i++)
			if(target[i] == prob.y[i])
				++total_correct;
		printf("Cross Validation Accuracy = %g%%\n",100.0*total_correct/prob.l);
	}

	free(target);
}

void parse_command_line(int argc, char **argv, char *input_file_name, char *model_file_name)
{
	int i;
	void (*print_func)(const char*) = NULL;	// default printing to stdout

	// default values
	param.solver_type = L2R_L2LOSS_SVC_DUAL;
	param.C = 1;
	param.eps = INF; // see setting below
	param.p = 0.1;
	param.nr_weight = 0;
	param.weight_label = NULL;
	param.weight = NULL;
	param.init_sol = NULL;
	flag_cross_validation = 0;
	flag_C_specified = 0;
	flag_solver_specified = 0;
	flag_find_C = 0;
	bias = -1;

	// parse options
	for(i=1;i<argc;i++)
	{
		if(argv[i][0] != '-') break;
		if(++i>=argc)
			exit(1);
		switch(argv[i-1][1])
		{
			case 's':
				param.solver_type = atoi(argv[i]);
				flag_solver_specified = 1;
				break;

			case 'c':
				param.C = atof(argv[i]);
				flag_C_specified = 1;
				break;

			case 'p':
				param.p = atof(argv[i]);
				break;

			case 'e':
				param.eps = atof(argv[i]);
				break;

			case 'B':
				bias = atof(argv[i]);
				break;

			case 'w':
				++param.nr_weight;
				param.weight_label = (int *) realloc(param.weight_label,sizeof(int)*param.nr_weight);
				param.weight = (double *) realloc(param.weight,sizeof(double)*param.nr_weight);
				param.weight_label[param.nr_weight-1] = atoi(&argv[i-1][2]);
				param.weight[param.nr_weight-1] = atof(argv[i]);
				break;

			case 'v':
				flag_cross_validation = 1;
				nr_fold = atoi(argv[i]);
				if(nr_fold < 2)
				{
					fprintf(stderr,"n-fold cross validation: n must >= 2\n");
					exit(1);
				}
				break;

			case 'q':
				print_func = &print_null;
				i--;
				break;

			case 'C':
				flag_find_C = 1;
				i--;
				break;

			default:
				fprintf(stderr,"unknown option: -%c\n", argv[i-1][1]);
				exit(1);
				break;
		}
	}

	set_print_string_function(print_func);

	// determine filenames
	if(i>=argc)
		exit(1);

	strcpy(input_file_name, argv[i]);

	if(i<argc-1)
		strcpy(model_file_name,argv[i+1]);
	else
	{
		char *p = strrchr(argv[i],'/');
		if(p==NULL)
			p = argv[i];
		else
			++p;
		sprintf(model_file_name,"%s.model",p);
	}

	// default solver for parameter selection is L2R_L2LOSS_SVC
	if(flag_find_C)
	{
		if(!flag_cross_validation)
			nr_fold = 5;
		if(!flag_solver_specified)
		{
			fprintf(stderr, "Solver not specified. Using -s 2\n");
			param.solver_type = L2R_L2LOSS_SVC;
		}
		else if(param.solver_type != L2R_LR && param.solver_type != L2R_L2LOSS_SVC)
		{
			fprintf(stderr, "Warm-start parameter search only available for -s 0 and -s 2\n");
			exit(1);
		}
	}

	if(param.eps == INF)
	{
		switch(param.solver_type)
		{
			case L2R_LR:
			case L2R_L2LOSS_SVC:
				param.eps = 0.01;
				break;
			case L2R_L2LOSS_SVR:
				param.eps = 0.001;
				break;
			case L2R_L2LOSS_SVC_DUAL:
			case L2R_L1LOSS_SVC_DUAL:
			case MCSVM_CS:
			case L2R_LR_DUAL:
				param.eps = 0.1;
				break;
			case L1R_L2LOSS_SVC:
			case L1R_LR:
				param.eps = 0.01;
				break;
			case L2R_L1LOSS_SVR_DUAL:
			case L2R_L2LOSS_SVR_DUAL:
				param.eps = 0.1;
				break;
		}
	}
}

void read_problem_mem(std::vector<std::string> &tag, std::vector<std::vector<float>> &svec)
{
	int max_index, inst_max_index, i;
	size_t elements, j;
    size_t k;
    double label;
    int index = 1;
    int n_feature = svec[0].size();
    
    elements = tag.size() * (n_feature+1);
    prob.l = tag.size();

	prob.bias=bias;

	prob.y = Malloc(double,prob.l);
	prob.x = Malloc(struct feature_node *,prob.l);
	x_space = Malloc(struct feature_node,elements+prob.l);

	max_index = 0;
	j=0;
    k=0;
	for(i=0;i<prob.l;i++)
	{
		inst_max_index = 0; // strtol gives 0 if wrong format
		prob.x[i] = &x_space[j];
        label = atof(tag[i].c_str());
        
		prob.y[i] = label;

		while(1)
		{
			errno = 0;
			x_space[j].index = index;

            if(x_space[j].index <= inst_max_index)
				exit(1);
			else
				inst_max_index = x_space[j].index;

			errno = 0;
			x_space[j].value = svec[i][(k % n_feature)];

            ++index;
			++k;
            ++j;

            if((k % n_feature) == 0) break;
		} // index and values

		if(inst_max_index > max_index)
			max_index = inst_max_index;

		if(prob.bias >= 0)
			x_space[j++].value = prob.bias; //bias

		x_space[j++].index = -1; // index for bias
        index = 1;
        k=0;
	}

	if(prob.bias >= 0)
	{
		prob.n=max_index+1;
		for(i=1;i<prob.l;i++)
			(prob.x[i]-2)->index = prob.n;
		x_space[j-2].index = prob.n;
	}
	else
		prob.n=max_index;

}

}//namespace training

namespace predicting{
int print_null(const char *s,...) {return 0;}

static int (*info)(const char *fmt,...) = &printf;

struct feature_node *x;
int max_nr_attr = 64;

struct model* model_;
int flag_predict_probability=0;

void exit_input_error(int line_num)
{
	fprintf(stderr,"Wrong input format at line %d\n", line_num);
	exit(1);
}

static char *line = NULL;
static int max_line_len;

static char* readline(FILE *input)
{
	int len;

	if(fgets(line,max_line_len,input) == NULL)
		return NULL;

	while(strrchr(line,'\n') == NULL)
	{
		max_line_len *= 2;
		line = (char *) realloc(line,max_line_len);
		len = (int) strlen(line);
		if(fgets(line+len,max_line_len-len,input) == NULL)
			break;
	}
	return line;
}

int do_one_predict(std::vector<std::string> &tag, std::vector<std::vector<float>> &svec)
{
    if(tag.size() != 1 || svec.size() != 1) {
        std::cout << "Wrong use of do_one_predict function.\n";
        exit(1);
    }
    int correct = 0;
	int total = 0;
	double error = 0;
	double sump = 0, sumt = 0, sumpp = 0, sumtt = 0, sumpt = 0;    
    
	int nr_class=get_nr_class(model_);
	double *prob_estimates=NULL;
	int j, n;
	int nr_feature=get_nr_feature(model_);
	if(model_->bias>=0)
		n=nr_feature+1;
	else
		n=nr_feature;
	if(flag_predict_probability)
	{
		int *labels;

		if(!check_probability_model(model_))
		{
			fprintf(stderr, "probability output is only supported for logistic regression\n");
			exit(1);
		}

		labels=(int *) malloc(nr_class*sizeof(int));
		get_labels(model_,labels);
		prob_estimates = (double *) malloc(nr_class*sizeof(double));
		free(labels);
	}

	max_line_len = 1024;
	line = (char *)malloc(max_line_len*sizeof(char));
    int p = 0;
    int q = 1;
    for(p = 0; p< tag.size(); p++)
	{
		int i = 0;
		double target_label, predict_label;
		int inst_max_index = 0; // strtol gives 0 if wrong format

        target_label = atof(tag[p].c_str());

		//target_label = strtod(label,&endptr);
		//if(endptr == label || *endptr != '\0')
		//	exit_input_error(total+1);

		while(1)
		{
			if(i>=max_nr_attr-2)	// need one more for index = -1
			{
				max_nr_attr *= 2;
				x = (struct feature_node *) realloc(x,max_nr_attr*sizeof(struct feature_node));
			}

			errno = 0;
			x[i].index = (i % nr_feature)+1;
            
			if(x[i].index <= inst_max_index)
				exit_input_error(total+1);
			else
				inst_max_index = x[i].index;

			errno = 0;
			x[i].value = svec[p][(i % nr_feature)];
 
			// feature indices larger than those in training are not used
            i++;
            if((i % nr_feature) == 0) break;
            
		}

		if(model_->bias>=0)
		{
			x[i].index = n;
			x[i].value = model_->bias;
			i++;
		}
		x[i].index = -1;

		if(flag_predict_probability)
		{
			int j;
			predict_label = predict_probability(model_,x,prob_estimates);
		}
		else
		{
			predict_label = predict(model_,x);
		}

		if(predict_label == target_label)
			++correct;
		error += (predict_label-target_label)*(predict_label-target_label);
		sump += predict_label;
		sumt += target_label;
		sumpp += predict_label*predict_label;
		sumtt += target_label*target_label;
		sumpt += predict_label*target_label;
		++total;
	}
	if(check_regression_model(model_))
	{
		info("Mean squared error = %g (regression)\n",error/total);
		info("Squared correlation coefficient = %g (regression)\n",
			((total*sumpt-sump*sumt)*(total*sumpt-sump*sumt))/
			((total*sumpp-sump*sump)*(total*sumtt-sumt*sumt))
			);
	}
	else
		info("Accuracy = %g%% (%d/%d)\n",(double) correct/total*100,correct,total);
	if(flag_predict_probability)
		free(prob_estimates);

    free(prob_estimates);
    
    return correct;
}

void do_predict(std::vector<std::string> &tag, std::vector<std::vector<float>> &svec)
{

    FILE *output;
    int correct = 0;
	int total = 0;
	double error = 0;
	double sump = 0, sumt = 0, sumpp = 0, sumtt = 0, sumpt = 0;

    output = fopen("KLD.output","w");
    
	int nr_class=get_nr_class(model_);
	double *prob_estimates=NULL;
	int j, n;
	int nr_feature=get_nr_feature(model_);
	if(model_->bias>=0)
		n=nr_feature+1;
	else
		n=nr_feature;
	if(flag_predict_probability)
	{
		int *labels;

		if(!check_probability_model(model_))
		{
			fprintf(stderr, "probability output is only supported for logistic regression\n");
			exit(1);
		}

		labels=(int *) malloc(nr_class*sizeof(int));
		get_labels(model_,labels);
		prob_estimates = (double *) malloc(nr_class*sizeof(double));
		fprintf(output,"labels");
		for(j=0;j<nr_class;j++)
			fprintf(output," %d",labels[j]);
		fprintf(output,"\n");
		free(labels);
	}

	max_line_len = 1024;
	line = (char *)malloc(max_line_len*sizeof(char));
    int p = 0;
    int q = 1;
    for(p = 0; p< tag.size(); p++)
	{
		int i = 0;
		double target_label, predict_label;
		char *idx, *val, *label, *endptr;
		int inst_max_index = 0; // strtol gives 0 if wrong format

        target_label = atof(tag[p].c_str());

		//target_label = strtod(label,&endptr);
		//if(endptr == label || *endptr != '\0')
		//	exit_input_error(total+1);

		while(1)
		{
			if(i>=max_nr_attr-2)	// need one more for index = -1
			{
				max_nr_attr *= 2;
				x = (struct feature_node *) realloc(x,max_nr_attr*sizeof(struct feature_node));
			}

			errno = 0;
			x[i].index = (i % nr_feature)+1;
            
			if(x[i].index <= inst_max_index)
				exit_input_error(total+1);
			else
				inst_max_index = x[i].index;

			errno = 0;
			x[i].value = svec[p][(i % nr_feature)];
 
			// feature indices larger than those in training are not used
            i++;
            if((i % nr_feature) == 0) break;
            
		}

		if(model_->bias>=0)
		{
			x[i].index = n;
			x[i].value = model_->bias;
			i++;
		}
		x[i].index = -1;

		if(flag_predict_probability)
		{
			int j;
			predict_label = predict_probability(model_,x,prob_estimates);
			fprintf(output,"%g",predict_label);
			for(j=0;j<model_->nr_class;j++)
				fprintf(output," %g",prob_estimates[j]);
			fprintf(output,"\n");
		}
		else
		{
			predict_label = predict(model_,x);
			fprintf(output,"%g\n",predict_label);
		}

		if(predict_label == target_label)
			++correct;
		error += (predict_label-target_label)*(predict_label-target_label);
		sump += predict_label;
		sumt += target_label;
		sumpp += predict_label*predict_label;
		sumtt += target_label*target_label;
		sumpt += predict_label*target_label;
		++total;
	}
	if(check_regression_model(model_))
	{
		info("Mean squared error = %g (regression)\n",error/total);
		info("Squared correlation coefficient = %g (regression)\n",
			((total*sumpt-sump*sumt)*(total*sumpt-sump*sumt))/
			((total*sumpp-sump*sump)*(total*sumtt-sumt*sumt))
			);
	}
	else
		info("Accuracy = %g%% (%d/%d)\n",(double) correct/total*100,correct,total);
	if(flag_predict_probability)
		free(prob_estimates);
}

void exit_with_help()
{
	printf(
	"Usage: predict [options] test_file model_file output_file\n"
	"options:\n"
	"-b probability_estimates: whether to output probability estimates, 0 or 1 (default 0); currently for logistic regression only\n"
	"-q : quiet mode (no outputs)\n"
	);
	exit(1);
}


struct model *load_model_mem(mParam *mparams)
{
	int i;
	int nr_feature;
	int n;
	int nr_class;
	double bias;
	model *model_;
    model_ = Malloc(model,1);
	parameter& param = model_->param;

	model_->label = NULL;

	char *old_locale = setlocale(LC_ALL, NULL);
	if (old_locale)
	{
		old_locale = strdup(old_locale);
	}
	setlocale(LC_ALL, "C");

    for(int i=0;solver_type_table[i];i++) {
        if(strcmp(solver_type_table[i],mparams -> solver_type.c_str()) == 0)
            {
                param.solver_type = i;
                break;
            }
    }
    model_ -> nr_class = mparams -> nr_class;
    model_ -> nr_feature = mparams -> nr_feature;
    model_ -> bias = mparams -> bias;
    nr_class = model_->nr_class;
    model_->label = Malloc(int,nr_class);

	nr_feature=model_->nr_feature;
	if(model_->bias>=0)
		n=nr_feature+1;
	else
		n=nr_feature;
	int w_size = n;
	int nr_w;
	if(nr_class==2 && param.solver_type != MCSVM_CS)
		nr_w = 1;
	else
		nr_w = nr_class;

    for(int i=0;i<nr_class;i++)
        model_ -> label[i] = mparams -> label[i];
    
	model_->w=Malloc(double, w_size*nr_w);
	for(i=0; i<w_size; i++)
	{
		int j;
		for(j=0; j<nr_w; j++)
			model_ -> w[i*nr_w+j] = mparams -> w[i*nr_w+j];
	}
	setlocale(LC_ALL, old_locale);
	free(old_locale);

	return model_;
}

int onePredict(std::vector<std::string> &tag, std::vector<std::vector<float>> &svec, mParam *mparams)
{
    int cargc;
    char *cargv[100];

    std::cout << "onePredict" << std::endl;
    cargv[0] = (char *)"./test";
    cargv[1] = (char *)"test_train.txt";
    cargv[2] = (char *)"KLD.model";
    cargv[3] = (char *)"KLD.output";
    cargc = 4;


	int i;

	// parse options
	for(i=1;i<cargc;i++)
	{
		if(cargv[i][0] != '-') break;
		++i;
		switch(cargv[i-1][1])
		{
			case 'b':
				flag_predict_probability = atoi(cargv[i]);
				break;
			case 'q':
				info = &print_null_p;
				i--;
				break;
			default:
				fprintf(stderr,"unknown option: -%c\n", cargv[i-1][1]);
				exit_with_help();
				break;
		}
	}
	if(i>=cargc)
		exit_with_help();

	x = (struct feature_node *) malloc(max_nr_attr*sizeof(struct feature_node));
    model_=load_model_mem(mparams);
	int result = do_one_predict(tag, svec);
	free_and_destroy_model(&model_);
	free(line);
	free(x);

    free(cargv);
    return result;

}


void mainPredict(std::vector<std::string> &tag, std::vector<std::vector<float>> &svec, mParam *mparams)
{
    int cargc;
    char *cargv[100];

    std::cout << "mainPredict" << std::endl;
    cargv[0] = (char *)"./test";
    cargv[1] = (char *)"test_train.txt";
    cargv[2] = (char *)"KLD.model";
    cargv[3] = (char *)"KLD.output";
    cargc = 4;


	int i;

	// parse options
	for(i=1;i<cargc;i++)
	{
		if(cargv[i][0] != '-') break;
		++i;
		switch(cargv[i-1][1])
		{
			case 'b':
				flag_predict_probability = atoi(cargv[i]);
				break;
			case 'q':
				info = &print_null_p;
				i--;
				break;
			default:
				fprintf(stderr,"unknown option: -%c\n", cargv[i-1][1]);
				exit_with_help();
				break;
		}
	}
	if(i>=cargc)
		exit_with_help();

	x = (struct feature_node *) malloc(max_nr_attr*sizeof(struct feature_node));
    model_=load_model_mem(mparams);
	do_predict(tag, svec);
	free_and_destroy_model(&model_);
	free(line);
	free(x);
}

}//namespace predicting
}//namespace svm
}//namespace tfkld
