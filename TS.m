classdef TS
    properties                                                              % Properties.
        l                                                                       % Left border.
        r                                                                       % Right border. [l, r)
        fs                                                                      % Sampling frequency, = 1 indicates discrete-time.
        value                                                                   % Signal value, length = (r - l) * fs.
    end
    
    methods (Access = private)                                              % Inline methods.

    end

    methods                                                                 % Member methods.
        % Construction method.
            % TS({l, r}, value, [fs]) - All parameters.
                % value can be a scalar.
            % TS({l}, value, [fs])    - Automatically generate r.
            % TS(value, [fs])         - Default l = 0.
            % TS(ts)                  - Copy.
        function obj = TS(varargin)
            narginchk(1, 3);
            if class(varargin{1}) == "cell"
                narginchk(2, 3);

                if nargin == 3
                    obj.fs = varargin{3};
                else
                    obj.fs = 1;
                end

                if length(varargin{2}) == 1 && length(varargin{1}) == 2
                    obj.value = varargin{2} * ones(1, (varargin{1}{2} - varargin{1}{1}) * obj.fs);
                else
                    obj.value = varargin{2};
                end

                obj.l = varargin{1}{1};
                if length(varargin{1}) == 2
                    obj.r = varargin{1}{2};
                else
                    obj.r = obj.l + length(obj.value) / obj.fs;
                end

                if length(obj.value) ~= (obj.r - obj.l) * obj.fs
                    error("Wrong length of 'value'");
                end
            elseif class(varargin{1}) == "TS"
                narginchk(1, 1);
                obj = varargin{1};
            elseif class(varargin{1}) == "double"
                narginchk(1, 2);
                obj = TS({0}, varargin{:});
            end
        end

        % Getter and setter.
        function obj = set.l(obj, mL)
            if fix(mL) == mL
                obj.l = mL;
            else
                error("'l' should be a integer.");
            end
        end
        function obj = set.r(obj, mR)
            if fix(mR) == mR
                obj.r = mR;
            else
                error("'r' should be a integer.");
            end
        end
        function obj = set.fs(obj, mFs)
            if fix(mFs) == mFs && mFs > 0
                obj.fs = mFs;
            else
                error("'fs' should be a positive integer.");
            end
        end
    end

    methods (Static)                                                        % Static methods.

    end
end
